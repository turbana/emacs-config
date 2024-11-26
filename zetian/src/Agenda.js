import { Fragment } from "react";
import { TIME_LOCALE } from "./constants.js";
import "./Agenda.css";


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
    var taskType;
    switch (task.type) {
        // NOTE: when adding new icons be sure and update the <rel> tag that
        // loads the material stylesheet in index.html
        case "scheduled": taskType = "schedule"; break;
        case "deadline":  taskType = "calendar_month"; break;
        default: taskType = "question_mark";
    }
    return (
        <div className={`task type-${task.type} state-${task.state}`}>
            <span className="time">{time}</span>
            <span className="type material-symbols-outlined">{taskType}</span>
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
        <div className="agenda">{rows}</div>
    );
}

export default Agenda;
