defmodule SlackBot.Worker do
  use GenServer
  require Logger

  defmodule State do
    defstruct slack: nil
  end

  # Client

  def start_link([slack: [token: token]]) do
    slack = SlackRtm.open!(token)
    GenServer.start_link(__MODULE__, %State{slack: slack})
  end

  def init(state = %State{slack: slack}) do
    spawn_link(__MODULE__, :receive_event, [slack])
    {:ok, state}
  end

  def terminate(reason, _state = %State{slack: slack}) do
    Logger.debug "terminate by #{reason}"
    case SlackRtm.close(slack) do
      :ok -> Logger.debug "Succeeded to close the connection"
      {:error, error} -> Logger.info "Failed to close the connection: #{error}"
    end
    :ok
  end

  # Server (callbacks)


  def receive_event(slack) do
    received = SlackRtm.recv!(slack)
    Logger.info "received: #{inspect received}"
    receive_event(slack)
  end

end
