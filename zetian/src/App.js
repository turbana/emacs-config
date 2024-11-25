import React, { Fragment, useEffect, useState } from "react";
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
    // var taskType = "⏱️";
    var taskType;
    switch (task.type) {
    case "scheduled": taskType = "📅"; break;
    case "deadline":  taskType = "⏰"; break;
    default: taskType = "❓";
    }
    return (
        <div className={`task type-${task.type} state-${task.state}`}>
            <span className="time">{time}</span>
            <span className="type">{taskType}</span>
            <span className={"priority box priority-" + task.priority}>{task.priority}</span>
            <span className="title">
                <a href={"./?note=" + task.id}>
                    {task.title}
                </a>
            </span>
            {task.effort && <span className="effort box">{task.effort}</span>}
        </div>
    );
}

function DateDivider({time}) {
    const dow = new Intl.DateTimeFormat(TIME_LOCALE, {weekday:"long"}).format(time)
    const day = time.getDate();
    const month = new Intl.DateTimeFormat(TIME_LOCALE, {month:"long"}).format(time);
    const year = time.getFullYear();
    const now = new Date();
    const today = time.getDate() == now.getDate()
          && time.getMonth() == now.getMonth()
          && time.getFullYear() == now.getFullYear();
    return (
        <div className={"date" + (today ? " today" : "")}>
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
            <Fragment key={`${task.type}-${task.id}`}>
                {extra}
                <AgendaTask task={task} />
            </Fragment>
        );
    });
    return (
        <div className="agenda" key="bar">{rows}</div>
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
          <Agenda tasks={agendaTasks} key="huh" />
    </div>
  );
}

export default App;
