
export function createTaskList(data) {
    var tasks = [];
    data.map((task) => {
        if (task.state != "TODO") return;
        if (task.scheduled !== "") {
            tasks.push(createTask(task, task.scheduled, "scheduled"));
        }
        if (task.deadline !== "") {
            tasks.push(createTask(task, task.deadline, "deadline"));
        }
    });
    return tasks.sort(taskSort);
}

function taskSort(t1, t2) {
    const time1 = t1.time.getTime();
    const time2 = t2.time.getTime();
    if (time1 != time2) {
        return time1 - time2;
    } else if (t1.priority != t2.priority) {
        return t1.priority.localeCompare(t2.priority);
    } else if (t1.effort != t2.effort) {
        return t1.effort.localeCompare(t2.effort);
    } else {
        return t1.title.localeCompare(t2.title);
    }
}

function createTask(item, time, type) {
    return {
        title: item.title,
        time: new Date(time),
        type: type,
        state: item.state,
        priority: item.priority,
        effort: item.effort,
        id: item.id
    };
}
