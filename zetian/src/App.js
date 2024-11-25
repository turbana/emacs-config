import React, { useEffect, useState } from "react";
import axios from "axios";

import { createTaskList } from './tasks.js';
import logo from './logo.svg';
import './App.css';

const TIME_LOCALE = "en-US";

function AgendaTask({task}) {
    var hours = task.time.getHours();
    var minutes = task.time.getMinutes();
    var time;
    if (hours == 0 && minutes == 0) {
        time = (<> &nbsp; </>);
    } else {
        if (hours < 10) hours = `0${hours}`;
        if (minutes < 10) minutes = `0${minutes}`;
        time = `${hours}:${minutes}`;
    }
    return (
        <div className={"task " + task.type}>
            <span className="time">{time}</span>
            <span className="priority">#{task.priority}</span>
            <span className="title">
                <span className="type">{task.type}::</span>
                <a href={"./?note=" + task.id}>
                    {task.title}
                </a>
            </span>
            <span className="effort">[{task.effort}]</span>
        </div>
    );
}

function DateDivider({time}) {
    const dow = new Intl.DateTimeFormat(TIME_LOCALE, {weekday:"long"}).format(time)
    const day = time.getDate();
    const month = new Intl.DateTimeFormat(TIME_LOCALE, {month:"long"}).format(time);
    const year = time.getFullYear();
    return (
        <div className="date">
            {dow}, {day} {month} {year}
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
            extra = <DateDivider time={task.time} />;
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
