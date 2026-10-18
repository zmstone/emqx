defmodule EMQXNifMQ.MixProject do
  use Mix.Project
  alias EMQXUmbrella.MixProject, as: UMP

  def project do
    [
      app: :emqx_nifmq,
      version: "6.3.0",
      build_path: "../../_build",
      compilers: Mix.compilers() ++ [:cargo],
      cargo_crates: [%{path: "native/emqx_nifmq", lib: "emqx_nifmq"}],
      erlc_options: UMP.strict_erlc_options(),
      erlc_paths: UMP.erlc_paths(),
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [extra_applications: UMP.extra_applications()]
  end

  def deps() do
    UMP.deps([
      {:emqx_mix_utils, in_umbrella: true, runtime: false},
      {:emqx, in_umbrella: true}
    ])
  end
end
