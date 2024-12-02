import { Fragment } from "react";
import { TIME_LOCALE } from "./constants.js";
import "./Agenda.css";

import Stack from "react-bootstrap/Stack";
import Container from 'react-bootstrap/Container';
import Row from 'react-bootstrap/Row';
import Col from 'react-bootstrap/Col';

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
        case "scheduled": taskType = "schedule"; break;
        case "deadline":  taskType = "calendar_month"; break;
        default: taskType = "question_mark";
    }
    return (
        <Row>
            <Col>
                <Stack className={`task type-${task.type} state-${task.state}`}
                       direction="horizontal" gap={1}>
                    <div className="time">{time}</div>
                    <div className="type">
                        <span className="material-symbols-outlined">
                            {taskType}
                        </span>
                    </div>
                    <div className={"priority box priority-" + task.priority}>
                        {task.priority}
                    </div>
                    <div className="title">
                        <a href={"./?note=" + task.id}>
                            {task.title}
                        </a>
                    </div>
                    <div className="effort box ms-auto">{task.effort}</div>
                </Stack>
            </Col>
        </Row>
    )
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
    const overdue = time < now && !today;
    return (
        <Row>
            <Col className={"date" + (today ? " today" : "") + (overdue ? " overdue" : "")}>
                {dow}, {day} {month} {year} {overdue ? "(overdue)" : ""}
            </Col>
        </Row>
    );
}

function Agenda({tasks}) {
    var currentDate = null;
    const rows = tasks.map((task) => {
        var dividor = null;
        const date = task.time.toLocaleDateString();
        if (date != currentDate) {
            currentDate = date;
            dividor = <DateDivider time={task.time} />;
        }
        return (
            <Fragment key={`${task.type}-${task.id}`}>
                {dividor}
                <AgendaTask task={task} />
            </Fragment>
        );
    });
    return (
        <Stack gap={0}>
            {rows}
        </Stack>
    );
}

export default Agenda;
