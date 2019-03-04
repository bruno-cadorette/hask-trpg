var selection = undefined

const gameId = 1
const player = 1
var borders = {};
var countiesInfo = {}
var turnActions = []


function selectCounty(newElem) {
    if (selection) {
        selection.classList.remove("selected");
    }

    if (selection && selection.id === newElem.id) {
        cancelSelection();
    }
    else if (selection && areNeighboor(selection.id, newElem.id)) {
        move(selection.id, newElem.id)
        cancelSelection();
    }
    else {
        newElem.classList.add("selected");

        selection = newElem
        deactivateNonSelectionable(newElem.id)
    }
}

function move(id1, id2) {
    if (countiesInfo[id1].faction != player) {
        alert("You can't move thoses troops because it belong to another faction")
        return
    }
    let migratingPopulation = prompt("How much troop do you want to move? (origin: " + countiesInfo[id1].population + "), destination: " + countiesInfo[id2].population + ")")
    if (!migratingPopulation) {
        return
    }
    if (migratingPopulation > countiesInfo[id1].population) {
        move(id1, id2)
    }
    else {
        addAction({ origin: id1, destination: id2, troops: +migratingPopulation })
    }
}
function addAction(action){
    turnActions.push(action);
    let li = document.createElement("li");
    li.innerText = action.origin + " -> " + action.destination + " with " + action.troops + " troops"

    document.getElementById("actions").appendChild(li)
}

function areNeighboor(id1, id2) {
    return borders[id1].includes(id2)
}

function cancelSelection() {
    let allCounty = document.getElementsByClassName("county")
    for (var i = 0; i < allCounty.length; i++) {
        allCounty[i].classList.remove("inacessible")
    }
    selection = undefined
}

function commit(){
    console.log(turnActions)
    postGameByGameIdGameState(gameId, turnActions, g =>{
        countiesInfo = g
        turnActions = []
        document.getElementById("actions").innerHTML = ""
        updateMap()
    },console.log);
}
function getPositionFromId(id) {
    return id.split("_")
}
function deactivateNonSelectionable(id) {
    let neighboors = borders[id];

    let allCounty = document.getElementsByClassName("county")

    for (var i = 0; i < allCounty.length; i++) {
        if (neighboors.includes(allCounty[i].id)) {
            allCounty[i].classList.remove("inacessible")
        }
        else {
            allCounty[i].classList.add("inacessible")
        }
    }
}

function drawMap() {
    const game = document.getElementById("game");
    for (var i = 0; i < 15; i++) {
        let row = document.createElement("tr");
        for (var j = 0; j < 15; j++) {
            let td = document.createElement("td");

            let button = document.createElement("div")

            button.classList.add("county")
            button.onclick = ev => {
                selectCounty(ev.target)
                console.log(ev.target.id)
            }
            button.id = i + "_" + j
            td.appendChild(button);
            row.appendChild(td)
        }
        game.appendChild(row);
    }
}

function updateMap() {
    
    console.log(countiesInfo)
    let allCounty = document.getElementsByClassName("county")
    for (var i = 0; i < allCounty.length; i++) {
        var info = countiesInfo[allCounty[i].id]
        allCounty[i].dataset.faction = info.faction;
        allCounty[i].innerHTML = info.population;
    }
}

function init(){
    console.log("init")
    getGameByGameIdBorders(gameId, b => {
        borders = b
        getGameByGameIdGameState(gameId, g => {
            countiesInfo = g
            drawMap();
            updateMap()
        })
    })
}