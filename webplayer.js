// Function to parse query arguments from the URL
function getQueryParameter(param, defaultValue) {
    const params = new URLSearchParams(window.location.search);
    return params.get(param) || defaultValue;
}

// Get the host and port from the query parameters
const host = getQueryParameter('host', 'localhost');
const port = getQueryParameter('port', '6839');

// Create the WebSocket URL
const websocketUrl = `ws://${host}:${port}`;

let socket;
let retryInterval = 3000; // Retry interval in milliseconds

function connectWebSocket() {
    currentAttempt = null;
    
    console.log(`Attempting to connect to WebSocket at ${websocketUrl}...`);
    socket = new WebSocket(websocketUrl);

    // Event handlers
    socket.onopen = () => {
        console.log('WebSocket connection opened to', websocketUrl);
    };

    socket.onmessage = (event) => {
        console.log('Message received:', event.data);
        newSlide(JSON.parse(event.data));
    };

    socket.onclose = (event) => {
        console.warn('WebSocket connection closed:', event.reason || 'no reason provided');
        retryConnection();
    };

    socket.onerror = (error) => {
        console.error('WebSocket error:', error);
        retryConnection();
    };
}

let currentAttempt;

function retryConnection() {
    if (!currentAttempt) {
        console.log(`Retrying connection in ${retryInterval / 1000} seconds...`);
        currentAttempt = setTimeout(connectWebSocket, retryInterval);
    }
}

// Start the initial connection
connectWebSocket();


function newSlide({command, ...data})
{
    if (command === "newslide")
    {
        var {comment, lines} = data;
        for (var i=0; ; i++)
        {
            elt = document.getElementById(`line${i}`)
            if (elt === null) break;
            elt.textContent = (i < lines.length) ? lines[i] : "";
        }
    }
    if (command === "hide")
    {
        var elt = document.querySelector(".lines");
        elt.classList.add("hidden");
    }
    if (command === "show")
    {
        var elt = document.querySelector(".lines");
        elt.classList.remove("hidden");
    }
    if (command === "style")
    {
        var {style} = data;
        var stylesheet = document.styleSheets[0];
        if (stylesheet.cssRules.length > 0) stylesheet.deleteRule(0);
        stylesheet.insertRule(`.globalStyle { ${style} }`);
    }
}

// Let the text box be dragged
document.addEventListener("DOMContentLoaded", () => {
    console.log("loaded");
    var linesBox = document.getElementById("lines");
    
    linesBox.addEventListener("mousedown", (event) => {
        console.log("Mouse down");
        var dragStartX = event.pageX;
        var dragStartY = event.pageY;
        console.log(`Original coords are ${linesBox.style.left}, ${linesBox.style.top}`);
        var posStartLeft = parseFloat(linesBox.style.left) || 0;
        var posStartTop = parseFloat(linesBox.style.top) || 0;
        console.log(`Original coords are ${posStartLeft}, ${posStartTop}`);
        var moveListener = (event) => {
            console.log("Mouse move");
            linesBox.style.left = `${posStartLeft + (event.pageX - dragStartX)}px`;
            linesBox.style.top = `${posStartTop + (event.pageY - dragStartY)}px`;
            event.preventDefault();
        }
        document.addEventListener("mousemove", moveListener, true);
        document.addEventListener("mouseup", (event) => {
            console.log("Mouse up");
            document.removeEventListener("mousemove", moveListener, true);
        }, {once: true, capture: true});
    });
    
});


