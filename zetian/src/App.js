import React, { useEffect, useState } from "react";
import axios from "axios";

import { createTaskList } from './tasks.js';
import logo from './logo.svg';
import './App.css';

function AgendaTask({task}) {
    return (
        <div className={"task " + task.type}>
            <span className="priority">#{task.priority}</span>
            <span className="title">
                <span className="type">{task.type}::</span>
                <a href={"./?note=" + task.id}>
                    {task.title}
                </a>
            </span>
            <span className="effort">[{task.effort}]</span>
            <span className="time">{task.time.toLocaleTimeString()}</span>
        </div>
    );
}

function Agenda({tasks}) {
    var currentDate = null;
    const rows = tasks.map((task) => {
        var extra = null;
        const date = task.time.toLocaleDateString();
        if (date != currentDate) {
            currentDate = date;
            extra = <div>{date}</div>;
        }
        return (
            <>
                {extra}
                <AgendaTask task={task} />
            </>
        );
    });
    return (
            <div className="agenda">{rows}</div>
    );
}

function App() {
    const [agendaTasks, setAgendaTasks] = useState([]);

    useEffect(() => {
        axios.get("agenda.json").then((res) => {
            setAgendaTasks(createTaskList(res.data.tasks));
        });
    }, []);
    
  return (
    <div className="App">
          <Agenda tasks={agendaTasks} />
    </div>
  );
}

export default App;
