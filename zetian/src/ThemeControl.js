import { useState } from 'react';
import Nav from 'react-bootstrap/Nav';

import "./ThemeControl.css";

function ThemeControl() {
    const [theme, setTheme] = useState(true);

    function handleClick() {
        const next = !theme;
        const mode = next ? "light" : "dark";
        console.log(`setting theme to ${mode}`);
        setTheme(next);
        document.documentElement.setAttribute("data-bs-theme", mode);
    }
    
    return (
        <Nav.Link onClick={handleClick}>
        <span className="material-symbols-outlined">
            {theme ? "dark_mode" : "light_mode"}
        </span>
        </Nav.Link>
    );

}

export default ThemeControl;
