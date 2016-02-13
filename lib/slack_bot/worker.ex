defmodule SlackBot.Worker do
  use GenServer
  require Logger

  defmodule State do
    defstruct slack: nil
  end

  def start_link(token) do
    slack = SlackRtm.open!(token)
    {:ok, _pid} = GenServer.start_link(__MODULE__, %State{slack: slack}, name: SlackBot.Worker)
  end

  def init(state = %State{slack: slack}) do
    _pid = spawn_link(__MODULE__, :receive_event, [slack])
    {:ok, state}
  end

  def handle_call(:me, _from, state = %State{slack: slack}) do
    {:reply, slack.self, state}
  end

  def handle_cast(%SlackBot.SendMessage{channel: channel, message: message}, state = %State{slack: slack}) do
    {:ok, new_slack} = SlackRtm.send!(slack, message, channel)
    {:noreply, %State{state | slack: new_slack}}
  end

  def terminate(reason, _state = %State{slack: slack}) do
    Logger.debug "terminate by #{inspect reason}"
    case SlackRtm.close(slack) do
      :ok -> Logger.debug "Succeeded to close the connection"
      {:error, error} -> Logger.info "Failed to close the connection: #{inspect error}"
    end
    :ok
  end

  def receive_event(slack) do
    received = SlackRtm.recv!(slack)
    Logger.debug "received: #{inspect received}"
    GenServer.cast(SlackBot.PluginManager, received)
    receive_event(slack) # loop
  end
end
