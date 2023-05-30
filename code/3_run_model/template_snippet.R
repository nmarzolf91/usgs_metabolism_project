    ## now we set up the priors. you can see their defaults with `?specs`.
    ## we need priors for the centers of the Q bins in the Q-K600 relationship,
    ## and for the means and SDs of K600.
    Q_by_day = tapply(log(d$discharge),
                      substr(d$solar.time, 1, 10),
                      mean)

    #establish node centers as an even sequence from the min to the max daily log Q.
    #this can be refined so that the nodes better represent the bulk of your data.
    K600_lnQ_nodes_centers = seq(from=min(Q_by_day, na.rm=TRUE),
                                 to=max(Q_by_day, na.rm=TRUE),
                                 by = 0.2)
    n_nodes = length(K600_lnQ_nodes_centers)


    #officially set all specs, including priors
    bayes_specs_new = streamMetabolizer::specs(
        model_name=bayes_name_new,
        K600_lnQ_nodes_centers = K600_lnQ_nodes_centers,
        K600_lnQ_nodes_meanlog = ???,
        K600_lnQ_nodes_sdlog = rep(0.1, n_nodes),
        K600_daily_sigma_mean = 0, #is this even a settable parameter?
        K600_daily_sigma_sigma = median_k600d_from_mle_run * 0.02

