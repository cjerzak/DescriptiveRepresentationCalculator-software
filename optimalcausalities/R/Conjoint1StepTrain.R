#' ml_train
#'
#' Implements the organizational record linkage algorithms of Jerzak and Libgober (2021).
#'
#' @usage
#'
#' ml_train(x, y, by ...)
#'
#' @param x,y data frames to be merged
#'
#' @return `z` The merged data frame.
#' @export
#'
#' @details `LinkOrgs` automatically processes the name text for each dataset (specified by `by`, `by.x`, and/or `by.y`. Users may specify the following options:
#'
#' - Set `DistanceMeasure` to control algorithm for computing pairwise string distances. Options include "`osa`", "`jaccard`", "`jw`". See `?stringdist::stringdist` for all options. (Default is "`jaccard`")
#'
#' @examples
#'
#' #Create synthetic data
#' x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
#' y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")
#' x <- data.frame("orgnames_x"=x_orgnames)
#' y <- data.frame("orgnames_y"=y_orgnames)
#'
#' # Perform merge
#' linkedOrgs <- LinkOrgs(x = x,
#'                        y = y,
#'                        by.x = "orgnames_x",
#'                        by.y = "orgnames_y",
#'                        MaxDist = 0.6)
#'
#' print( linkedOrgs )
#'
#' @export
#'
#' @md



ml_train <- function(){

print("Begin training block...")
for(trainIndicator in trainIndicator_pool){
  # then iterate over folds
  for(fold_ in 1:(nFolds*trainIndicator+1*!trainIndicator)){

  # iterate over lambda FIRST (for warm starts...)

  lambda_counter <- 0; for(REGULARIZATION_LAMBDA in LAMBDA_SEQ){
        lambda_counter <- lambda_counter + 1

      # training indices pool
      if(trainIndicator == 1){
        availableTrainIndices <- split1_indices[holdBasis_traintv != fold_]
        holdIndices <- split1_indices[holdBasis_traintv == fold_]
      }
      if(trainIndicator == 0){
        if(warmStart){ nSGD <- nSGD * 3 }
        availableTrainIndices <- holdIndices <- split2_indices
      }

      # setup epochs and indices
      if(!is.null(nEpoch) & !is.null(nSGD)){ print("Defaulting to nEpoch over nSGD") }
      if(is.null(nEpoch) & !is.null(nSGD)){
        print("Inferring nEpoch")
        nEpoch <- ceiling(nSGD *  batch_size / length(availableTrainIndices))
      }
      availableTrainIndices_train_seq <- c(replicate(nEpoch,
                tapply(sample(availableTrainIndices),
                            1:length(availableTrainIndices) %%
                               round(length(availableTrainIndices)/batch_size),c) ))
      min_numb <- max(1,min(unlist(lapply(availableTrainIndices_train_seq,length)),na.rm=T))
      availableTrainIndices_train_seq <- lapply(availableTrainIndices_train_seq,function(zer){
        sample(zer,min_numb,replace=F) })
      print(sprintf("N Obs Per Iter: %i",min_numb))
      #table(unlist(lapply(availableTrainIndices_train_seq,length)))

      nSGD <-  length(availableTrainIndices_train_seq)
      print(sprintf("nSGD: %s, nEpoch: %s",nSGD,nEpoch))

      # obtain initialization
      if(fold_ == 1){ selected_bscale <- 1 }
      print(sprintf("Selected bscale: %.4f",selected_bscale))

      # Re-initialize trainable variables right before new training begins
      # IF warmstarts OFF or FOLD == 1 or TESTING
      if(REINIT_BOOL <- (!warmStart | lambda_counter == 1 | trainIndicator == F)){
        print("Reinitializing variables...")
        reinitialize_all(b_SCALE = selected_bscale)
      }
      if(!REINIT_BOOL){print("NOT reinitializing variables...")}

      # get initial LR
      L2_squared_init <- 1*median(replicate(5, {
          # generate batch indices
          my_batch <- availableTrainIndices_train_seq[[sample(1:length(availableTrainIndices_train_seq),1) ]]

          # train
          trainStep( y_  = as.matrix(Yobs[my_batch]),
                     x_  = X[my_batch,],
                     f_  = FactorsMat_numeric_0Indexed[my_batch,],
                     lp_ = as.matrix(log_pr_w[my_batch]),
                     lambda_ = returnWeightsFxn(REGULARIZATION_LAMBDA),
                     applyGradients = F)
          L2_norm_squared
      }))

      # initialize initial LR on this basis for adaptive LR methods
      if(sg_method %in% c("adanorm", "wngrad")){
        optimizer_tf$learning_rate$assign( sqrt( 1/L2_squared_init  ) )
      }

      # start training process
      AdagradLR_vec_eff <- AdagradLR_vec <- MomenetumNextIter_seq <- LR_effective <- InvLR_tracker <- L2_norm_squared_vec <- loss_vec <- rep(NA,times = nSGD)
      InvLR_tracker <- c(L2_squared_init, InvLR_tracker)
      {
        marginalProb_m <- c()
        i_eff <- 1; SquaredL2Grad_accumulations <- L2_squared_init
        for(i in 1:nSGD){
          # generate batch indices
          my_batch <- availableTrainIndices_train_seq[[i]]

          #get marginals
          #marginalProb_m <- rbind(marginalProb_m,t( colMeans(as.array(getClassProb(X[my_batch,]) ))))

          # train
          if(optimization_language == "tf"){
            trainStep( y_  = as.matrix(Yobs[my_batch]),
                       x_  = X[my_batch,],
                       f_  = FactorsMat_numeric_0Indexed[my_batch,],
                       lp_ = as.matrix( log_pr_w[my_batch] ),
                       lambda_ = returnWeightsFxn(REGULARIZATION_LAMBDA),
                       applyGradients = T)
          }

          if(optimization_language == "jax"){
            #my_batch_jax <- my_batch[ jax_batch_select <- (1:(length(my_batch) )) ]
            my_batch_jax <- my_batch[ jax_batch_select <- (1:(length(my_batch)-2L) ) ]

            # define training function via jax
            if(i == 1 & lambda_counter == 1){
              jax <- tensorflow::import("jax")
              tf2jax <- import("tf2jax")
              jnp <- tensorflow::import("jax.numpy")
              eq <- reticulate::import("equinox")
              optax <- reticulate::import("optax")
              kfac <- reticulate::import("kfac_jax")
              batch_size_jax <- length(my_batch_jax)

              # convert
              jax_fxn_raw <- tf2jax$convert(getLoss_tf, Y_  = tf$constant(as.matrix(Yobs[my_batch_jax]),tf$float32),
                                            X_  = tf$constant(X[my_batch_jax,],tf$float32),
                                            factorMat_  = tf$constant(FactorsMat_numeric_0Indexed[my_batch_jax,],tf$int32),
                                            logProb_ = tf$constant(as.matrix(log_pr_w[my_batch_jax]),tf$float32),
                                            REGULARIZATION_LAMBDA = tf$constant(returnWeightsFxn(REGULARIZATION_LAMBDA),tf$float32))
              param_set <- jax_fxn_raw[[2]]

              # convert fxn with eval+params output into eval only
              def_sig <- gsub(jax_fxn_raw[[1]]$signature,pattern="\\<Signature ",replace="")
              def_sig  <- gsub(gsub(def_sig,pattern ="\\(",replace=""),pattern="\\)",replace="")
              input_sig  <- gsub(gsub(def_sig,pattern ="\\(",replace=""),pattern="\\)",replace="")
              input_sig_sep <- strsplit(input_sig,split=", ")[[1]]
              jax_fxn <- sprintf('function(params,%s){
                                 out_ <- jax_fxn_raw[[1]](params,%s)[[1]];
                                 return( jnp$reshape(out_,list()) )
                                 }', def_sig ,input_sig)
              jax_fxn <- eval(parse(text = jax_fxn))
              v_and_grad_jax_fxn_raw <- jax$value_and_grad(jax_fxn,argnums = 0L)

              # compile
              jax_fxn <- jax$jit(  jax_fxn  )
              v_and_grad_jax_fxn <- jax$jit(   v_and_grad_jax_fxn_raw  )

              # test the function
              jax_eval <- jax_fxn(
                param_set,
                Y_  = jnp$array(as.matrix(Yobs[my_batch_jax])),
                X_  = jnp$array(X[my_batch_jax,]),
                factorMat_  = jnp$array(FactorsMat_numeric_0Indexed[my_batch_jax,]),
                logProb_ = jnp$array(as.matrix(log_pr_w[my_batch_jax])),
                REGULARIZATION_LAMBDA = jnp$array(returnWeightsFxn(REGULARIZATION_LAMBDA))
              )
              param_set_names <- names( param_set )
            }

            # need to reset param_set at i == 1
            if( i == 1 ){
              if(lambda_counter == 1){
                param_set_new_init <- lapply(tv_trainWith,function(zer){ as.array(  zer$value() )} )
                names(param_set_new_init) <- names( param_set )
              }
              param_set <- param_set_new_init;
              param_set <- lapply(param_set,function(zer){jnp$array(zer)})
              #plot(unlist(param_set))
              #cbind(names( param_set),unlist( lapply(tv_trainWith,function(zer){zer$name})))
              # setup optimizer
              OptimType <- "Other"
              if(OptimType == "SecondOrder"){
                hessian_fxn <- jax$jit( jax$hessian(jax_fxn,argnums = 0L) )
                optax_optimizer <-  optax$chain(
                  optax$adaptive_grad_clip(1, eps=0.0001),
                  optax$zero_nans(),
                  optax$scale(1) )
              }
              if(OptimType == "AdagradNorm"){
                  optax_optimizer <-  optax$chain(
                    optax$adaptive_grad_clip(0.25, eps=0.0001),
                    optax$scale(-1)
                  )
              }
              if(OptimType == "Other"){
                LR_schedule <- optax$warmup_cosine_decay_schedule(
                  init_value = (LEARNING_RATE_BASE<- .1)/2,
                  peak_value = LEARNING_RATE_BASE,
                  warmup_steps = nWarm <- 50L, decay_steps = nSGD - nWarm)
                optax_optimizer <-  optax$chain(
                  #optax$sgd(momentum = 0.90, nesterov = T,
                  #optax$scale_by_schedule(LR_schedule),
                  #optax$adaptive_grad_clip(0.5, eps=0.0001),
                  #optax$fromage(learning_rate = 0.1)
                  optax$clip(1.),
                  optax$scale_by_rss(), optax$scale(-1)
                  #optax$scale_by_rss(), optax$noisy_sgd(learning_rate = 1)
                  )
              }

              # model partition + setup state
              opt_state <- optax_optimizer$init( param_set )
              jit_apply_updates <- eq$filter_jit(optax$apply_updates)
              jit_update <- eq$filter_jit(optax_optimizer$update)
            }

            # fix jax training size due to compiled functionality
            my_batch_jax <- my_batch_jax[1:batch_size_jax]
            my_batch_jax[is.na(my_batch_jax)] <- my_batch_jax[sample(1:sum(!is.na(my_batch_jax)),
                                        size = length(my_batch_jax[is.na(my_batch_jax)]), replace = T)]

            {
            # updates + derivatives using jax
            v_and_grad_eval <- v_and_grad_jax_fxn( param_set[param_set_names],
                              Y_ = jnp$array(as.matrix(Yobs[my_batch_jax])),
                              X_ = jnp$array(X[my_batch_jax,]),
                              factorMat_ = jnp$array(FactorsMat_numeric_0Indexed[my_batch_jax,]),
                              logProb_ = jnp$array(as.matrix(log_pr_w[my_batch_jax])),
                              REGULARIZATION_LAMBDA = jnp$array(returnWeightsFxn(REGULARIZATION_LAMBDA)))

            # subset
            #if(lambda_counter > 1){browser()}
            currentLossGlobal <- v_and_grad_eval[[1]]$tolist()
            grad_set <- v_and_grad_eval[[2]] #jax$grad screws up name orders
            param_set_names_jax_order <- names(grad_set)
            L2_norm <- optax$global_norm(grad_set)$tolist()
            L2_norm_squared_vec[i] <- L2_norm_squared <- L2_norm^2

            # add noise to the gradients
            if(T == T){
              if(i == 1){
              JaxKey <- jax$jit(function(int_){ jax$random$PRNGKey(int_)})
              PercentJitterFxn <-  jax$jit( function(x,seed){
                abs_x_frac <- jnp$multiply(jnp$array(0.25),jnp$abs(x))
                ep_noise <- oryx$distributions$Uniform(jnp$negative(abs_x_frac),abs_x_frac)
                ep_noise <- ep_noise$sample(seed = JaxKey(seed))
                jnp$add(x,ep_noise)
              })  }
              grad_set <- jax$tree_map(function(z){ PercentJitterFxn(z,jnp$array(as.integer(runif(1,0,10000))))},grad_set)
            }

            if(OptimType == "AdagradNorm"){
              SquaredL2Grad_accumulations <- SquaredL2Grad_accumulations + L2_norm^2
              AdagradLR <-  jnp$array(  0.5 / SquaredL2Grad_accumulations^0.5 )
              AdagradLR_vec[i] <- AdagradLR$tolist()
              AdagradLR_vec_eff[i] <- AdagradLR_use <- AdagradLR_vec[i_eff]
              AdagradLR_use <- jnp$array(AdagradLR_use)
              grad_set <- jax$tree_util$tree_map(function(x){jnp$multiply(x,AdagradLR_use)}, grad_set)[names(grad_set)]
            }
            if(OptimType == "SecondOrder"){
              hessian_value <- hessian_fxn(param_set[param_set_names],
                                           Y_ = jnp$array(as.matrix(Yobs[my_batch_jax])),
                                           X_ = jnp$array(X[my_batch_jax,]),
                                           factorMat_ = jnp$array(FactorsMat_numeric_0Indexed[my_batch_jax,]),
                                           logProb_ = jnp$array(as.matrix(log_pr_w[my_batch_jax])),
                                           REGULARIZATION_LAMBDA = jnp$array(returnWeightsFxn(REGULARIZATION_LAMBDA)))
              HessianMat <- matrix(list(),nrow = length(param_set),ncol=length(param_set))
              row.names(HessianMat) <- colnames(HessianMat) <- names(hessian_value)
              for(jaa in names(hessian_value)){ for(ja in names(hessian_value)){
                HessianMat[jaa,ja] <- list(eval(parse(text = sprintf("jnp$squeeze(jnp$squeeze(hessian_value$`%s`$`%s`,1L),2L)",jaa,ja))))
              }}
              HessianMat <- apply(HessianMat,1,function(zer){ names(zer) <- NULL;
                              jnp$concatenate(zer,1L) })
              names(HessianMat) <- NULL; HessianMat <- jnp$concatenate(HessianMat,0L)

              if(i == 1){ HessianMat_AVE <- jnp$zeros(HessianMat$shape) }
              {
                  w_i_minus_1 <- (((i-1) + 1)^log((i-1)+1))
                  w_i <- (i + 1)^log(i+1)
                  HessianMomentum <- w_i_minus_1 / w_i
                  HessianMat_AVE <- jnp$add(jnp$multiply(HessianMomentum,HessianMat_AVE),
                                                jnp$multiply(1-HessianMomentum,HessianMat))
              }

              # get flat grad
              grad_vec <- jax$flatten_util$ravel_pytree( grad_set )

              # get inv hessian times grad
              #image( (jnp$linalg$inv(HessianMat_AVE)$to_py() ))
              #https://arxiv.org/pdf/2204.09266.pdf
              # armijo condition test
              rho <- 0.98
              NetwornDir <- jnp$negative( jnp$matmul(jnp$linalg$inv(
                jnp$add(HessianMat_AVE,jnp$multiply(0.5,jnp$identity(HessianMat$shape[[1]])))
                ), grad_vec[[1]]) )
              InnerProd_GradNewtonDir <- jnp$sum(jnp$multiply(NetwornDir, grad_vec[[1]]))$tolist()
              if(InnerProd_GradNewtonDir > 0){ grad_set <- grad_vec[[2]](jnp$zeros(NetwornDir$shape))}
              if(InnerProd_GradNewtonDir <= 0){
              armijo_count <- 0; go_on <- F; while(go_on == F){
                armijo_count <- armijo_count + 1
                SecondOrderUpdates <- jnp$multiply(mu_t <- rho^armijo_count,
                                                   NetwornDir)
                SecondOrderUpdates <-  grad_vec[[2]](SecondOrderUpdates)

                param_set_test <- jit_apply_updates(params = param_set,
                                    updates = SecondOrderUpdates)
                f_x_updated <- jax_fxn( param_set_test[param_set_names],
                                            Y_ = jnp$array(as.matrix(Yobs[my_batch_jax])),
                                            X_ = jnp$array(X[my_batch_jax,]),
                                            factorMat_ = jnp$array(FactorsMat_numeric_0Indexed[my_batch_jax,]),
                                            logProb_ = jnp$array(as.matrix(log_pr_w[my_batch_jax])),
                                            REGULARIZATION_LAMBDA = jnp$array(returnWeightsFxn(REGULARIZATION_LAMBDA)))
                armijo_cond <- f_x_updated$tolist() <= (currentLossGlobal + mu_t*0.25*InnerProd_GradNewtonDir )
                #if(is.na(armijo_cond)){browser()}
                if(armijo_count > 100){go_on <- T; SecondOrderUpdates <- grad_vec[[2]](jnp$zeros(NetwornDir$shape))}
                if(armijo_cond == T){ go_on <- T }
              }
              grad_set <- SecondOrderUpdates
            }
            }

            # perform model updates
            updates_and_opt_state <- jit_update(
              updates = grad_set,
              state = opt_state,
              params = param_set[param_set_names_jax_order])
            optax_updates <- updates_and_opt_state[[1]]
            opt_state <- updates_and_opt_state[[2]]

            param_set <- jit_apply_updates(params = param_set,
                      updates = optax_updates)[param_set_names]
            }
          if(OptimType == "AdagradNorm"){
              if(cycle_width < Inf){if(i %% cycle_width == 0 & i < nSGD / 2){ i_eff <- 1 } }
          }

          }

          # report on performance 1 + 4 times during training
          if(i %% max(round( nSGD/4 ),1) == 0 | i == 1){
            try_ <- try(print(sprintf("Iter %i - Fold %i - Lambda %i of %i - Current obj: %.3f",
                                      i, fold_, lambda_counter, length(LAMBDA_SEQ), currentLossGlobal)),T)
            if(class(try_) == "try-error"){browser()}
          }

          # cycle info
          if(cycle_width < Inf){if(i %% cycle_width == 0 & i < nSGD / 2){ i_eff <- 1 } }
          loss_vec[i] <- currentLossGlobal
          L2_norm_squared_vec[i] <- L2_norm_squared
          if(sg_method == "wngrad"){ InvLR_tracker[ i+1 ] <- InvLR_tracker[i] + L2_norm_squared / InvLR_tracker[i] }
          if(sg_method == "adanorm"){ InvLR_tracker[ i+1 ] <- InvLR_tracker[i] + L2_norm_squared }
          i_eff <- i_eff + 1;

          # AdaGrad-Norm https://arxiv.org/pdf/1806.01811.pdf
          if(sg_method %in% c("adanorm")){ LR_effective[i] <- 1/sqrt( InvLR_tracker[i_eff] ) }

          # WN Grad LR
          if(sg_method %in% c("wngrad")){ LR_effective[i] <- 1/InvLR_tracker[i_eff] }

          # cosine LR
          if(sg_method == "cosine"){LR_effective[i] <- LEARNING_RATE_BASE*abs(cos(i/nSGD*cycle_width)  )*(i<nSGD/2) + NA20(LEARNING_RATE_BASE*(i>=nSGD/2)/(i-nSGD/2)^0.2*(i>nSGD/2)) }
          if(optimization_language == "tf"){ optimizer_tf$learning_rate$assign( LR_effective[i] )}

          if(adaptiveMomentum == T){ #https://arxiv.org/pdf/2110.09057.pdf
            if(optimization_language == "tf"){
              x_k <- as.numeric( tf$concat((lapply(tv_trainWith, function(zer){tf$reshape(zer,-1L)})),0L))
              grad_k <- as.numeric( tf$concat((lapply(my_grads, function(zer){tf$reshape(zer,-1L)})),0L))
            }
            if(optimization_language == "jax"){
              x_k <- unlist( param_set)
              grad_k <- unlist( lapply(grad_set, function(zer){zer$tolist()}))
            }
            if(i >= 3){
              DENOM <- sqrt( sum((x_k  -  x_k_minus_1)^2))
              NUM <- sqrt( sum((grad_k  -   grad_k_minus_1)^2))
              if(sg_method == "cosine"){ LR_current <- as.numeric( optimizer_tf$beta_1)}
              if(sg_method %in% c("wngrad","adanorm")){ LR_current <- as.numeric( optimizer_tf$learning_rate)}
              MomenetumNextIter_seq[i] <- MomenetumNextIter <- max(0., min( (1 - sqrt(LR_current * NUM / (0.000001+DENOM)))^2,
                                                                        1 - 10^(-2) ))
              if(sg_method == "cosine"){optimizer_tf$beta_1$assign(MomenetumNextIter)}
              if(sg_method == c("wngrad","adanorm")){optimizer_tf$momentum$assign(MomenetumNextIter)}
            }
            x_k_minus_1 <- x_k
            grad_k_minus_1 <- grad_k
          }
        }

        if(optimization_language=="jax"){
          # check names alignment
          print( sprintf("Is True? %s", 1 == mean(gsub(unlist( unlist( lapply(tv_trainWith,function(zer){zer$name}) )),pattern=":0",replace="") == names(param_set)) ))

          # make sure gets updated params
          lapply(1:length(tv_trainWith),function(zer){
            tv_trainWith[[zer]]$assign( param_set[[zer]] ) })
        }

        # figs to figure out dynamics
        {
          #try(plot(MomenetumNextIter_seq), T)
          try(plot(LR_effective), T)
          try(plot(L2_norm_squared_vec),T)
          try(plot(  loss_vec   ),T)
          try(points(lowess(loss_vec),type = "l",lwd = 5, col = "red"),T)
          if(kClust > 1){
            print(sprintf("Mean Abs Coef: %.5f:", mean(abs(as.array(ClassProbProj$kernel)))))
            try(plot( as.matrix(getClassProb(tfConst(X[availableTrainIndices,])))[,2] ),T)
          }
        }
      }
      # in sample objective

      try(plot(unlist( lapply(getPiList(),function(zer){ unlist(zer) - unlist(assignmentProbList) }) ) ),T)
      getQ_fxn <- function(indices_){
          # broken up indices
          # batch_indices_Q <- tapply(sample(indices_),1:length(indices_) %% round(length(indices_)/batch_size),c)

          # all together indices
          batch_indices_Q <- list( indices_ )
          Qhat_value <- mean( unlist( lapply(batch_indices_Q, function(use_i){
            finalWts_ <- prop.table( as.matrix(  getProbRatio_tf(Y_ = tfConst(as.matrix(Yobs[use_i])),
                                                                 X_ = tfConst(X[use_i,]),
                                                                 factorMat_ = tfConst(FactorsMat_numeric_0Indexed[use_i,],tf$int32),
                                                                 logProb_ = tfConst(as.matrix(log_pr_w[use_i])),
                                                                 REGULARIZATION_LAMBDA = tfConst(returnWeightsFxn(REGULARIZATION_LAMBDA))) ) )
            Qhat_ <- sum(as.matrix(Yobs[use_i])*finalWts_)
          } )))
          return( Qhat_value )
      }

      Qhat_inSamp <- getQ_fxn(  availableTrainIndices  )
      Qhat_hold  <- getQ_fxn(  holdIndices  )
      if(trainIndicator == 1){
        print("---Current Q: IN, OUT---");print(c(Qhat_inSamp,Qhat_hold))
        performance_matrix_out[fold_,lambda_counter] <- Qhat_hold
        performance_matrix_in[fold_,lambda_counter] <- Qhat_inSamp
        print( performance_matrix_out )
      }
    }
  }
  if(trainIndicator == 1){
    lowerBound_vec <- colMeans(performance_matrix_out) - 1 * apply(performance_matrix_out,2,function(zer){ sqrt(1/length(zer)*var(zer)) })
    LAMBDA_selected <- LAMBDA_SEQ <- LAMBDA_SEQ[which.max(lowerBound_vec)]
  }
}
}
