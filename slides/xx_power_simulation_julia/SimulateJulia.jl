using DataFrames
using MixedModels
using MixedModelsSim
using Random
using Statistics

function SimulateSimple(data, beta, theta, nsim = 1000, seed = 42)

    contrasts = Dict(:A => DummyCoding(base = "a1"),
                     :B => DummyCoding(base = "b1"))

    m1 = MixedModel(@formula(dv ~ 1 + A * B + (1 | id) + (1 | item)),
                    data, Bernoulli(); contrasts)

    simulate!(MersenneTwister(seed), m1; β = beta, θ = theta)
    fit!(m1)

    p1 = parametricbootstrap(MersenneTwister(seed), nsim, m1; β = beta, θ = theta)

    df_cond = groupby(DataFrame(p1.coefpvalues), :coefname)[4]
    
    return Dict(:pwr => mean(<(0.05), df_cond.p),
                :par => mean(df_cond.β),
                :se_boot => std(df_cond.β),
                :se_mean => mean(df_cond.se)
               )
end


function SimulateComplex(data, beta, theta, nsim = 1000, seed = 42)

    contrasts = Dict(:A => DummyCoding(base = "a1"),
                     :B => DummyCoding(base = "b1"))

    m1 = MixedModel(@formula(dv ~ 1 + A * B + (1 + B | id) + (1 + A| item)),
                    data, Bernoulli(); contrasts)

    simulate!(MersenneTwister(seed), m1; β = beta, θ = theta)
    fit!(m1)

    p1 = parametricbootstrap(MersenneTwister(seed), nsim, m1; β = beta, θ = theta)

    df_cond = groupby(DataFrame(p1.coefpvalues), :coefname)[4]
    
    return Dict(:pwr => mean(<(0.05), df_cond.p),
                :par => mean(df_cond.β),
                :se_boot => std(df_cond.β),
                :se_mean => mean(df_cond.se)
               )
end

