
export function createTaskList(data) {
    var tasks = [];
    data.map((task) => {
        if (task.scheduled !== "") {
            tasks.push(createTask(task, task.scheduled, "scheduled"));
        }
        if (task.deadline !== "") {
            tasks.push(createTask(task, task.deadline, "deadline"));
        }
    });
    return tasks.sort((t1, t2) => t2.time < t1.time);
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
