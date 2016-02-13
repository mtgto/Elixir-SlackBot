defmodule SlackBot do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    config = Mix.Config.read!("config/config.exs")

    children = [
      worker(SlackBot.Worker, [config[:slack_bot][:token]]),
      supervisor(SlackBot.PluginSupervisor, [config[:slack_bot][:plugins]]),
      worker(SlackBot.PluginManager, [%SlackBot.PluginManager.State{plugins: config[:slack_bot][:plugins], channels: config[:slack_bot][:channels]}])
    ]

    opts = [strategy: :one_for_one, name: SlackBot.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @spec send_message!(String.t, String.t, Maps.t) :: no_return
  def send_message!(channel, message, _attributes \\ %{}) do
    :ok = GenServer.cast(SlackBot.Worker, %SlackBot.SendMessage{channel: channel, message: message})
  end

  @doc """
  Return bot information.
  """
  @spec me :: Maps.t
  def me do
    GenServer.call(SlackBot.Worker, :me)
  end

  @doc """
  Return the text stripped bot name.

  For example, bot name is "bot" and text is "bot: hi, bot!".
  It returns "hi, bot!"
  """
  @spec strip_bot_name(String.t) :: {:ok, String.t} | {:error, :nomatch}
  def strip_bot_name(text) do
    name = me["name"]
    replaced = Regex.replace(~r/^#{name}(?:\: ?)?/, text, "")
    if text == replaced do
      {:error, :nomatch}
    else
      {:ok, replaced}
    end
  end
end
