import React, { useEffect, useState } from "react";
import axios from "axios";

import logo from './logo.svg';
import './App.css';

function AgendaTask({task}) {
    return (
            <div>{task.title}</div>
    );
}

function Agenda({tasks}) {
    const rows = tasks.map((task) => {
        return (
                <AgendaTask task={task} />
        );
    });
    return (
            <div>{rows}</div>
    );
}

function App() {
    const [agendaTasks, setAgendaTasks] = useState([]);

    useEffect(() => {
        axios.get("agenda.json").then((res) => {
            setAgendaTasks(res.data.tasks);
        });
    }, []);
    
  return (
    <div className="App">
          <Agenda tasks={agendaTasks} />
    </div>
  );
}

export default App;
