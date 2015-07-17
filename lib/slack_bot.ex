defmodule SlackBot do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    config = Mix.Config.read!("config/config.exs")

    children = [
      worker(SlackBot.Worker, [config[:slack][:token]]),
      supervisor(SlackBot.PluginSupervisor, [config[:slack][:plugins]]),
      worker(SlackBot.PluginManager, [%SlackBot.PluginManager.State{plugins: config[:slack][:plugins], channels: config[:slack][:channels]}])
    ]

    opts = [strategy: :one_for_one, name: SlackBot.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @spec send_message!(String.t, String.t, Maps.t) :: no_return
  def send_message!(channel, message, _attributes \\ %{}) do
    :ok = GenServer.cast(SlackBot.Worker, %SlackBot.SendMessage{channel: channel, message: message})
  end

  def me do
    GenServer.call(SlackBot.Worker, :me)
  end
end
