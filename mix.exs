defmodule SlackBot.Mixfile do
  use Mix.Project

  def project do
    [app: :slack_bot,
     version: "0.0.1",
     elixir: "~> 1.2",
     description: description,
     package: package,
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: [:logger, :slack_rtm]]
  end

  defp description do
    """
    Slack bot.
    """
  end

  defp deps do
    [{:slack_rtm, github: "mtgto/Elixir-SlackRTM"}]
  end

  defp package do
    [
      files: ["lib", "mix.exs", "README.md", "LICENSE"],
      contributors: ["mtgto"],
      licenses: ["The MIT License"],
      links: %{
        "GitHub" => "https://github.com/mtgto/Elixir-SlackRTM"
      }
    ]
  end
end
