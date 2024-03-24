// Función para convertir la tabla en formato CSV
function convertToCSV(table) {
    // Inicializar una variable para almacenar los datos CSV
    var csv = [];

    // Obtener todas las filas de la tabla
    var rows = table.querySelectorAll('.row');

    // Iterar sobre cada fila
    for (var i = 0; i < rows.length; i++) {
        var row = rows[i];
        var rowData = [];

        // Obtener todas las celdas de la fila actual
        var cells = row.querySelectorAll('.cell');

        // Iterar sobre cada celda
        for (var j = 0; j < cells.length; j++) {
            // Agregar el texto de la celda al array de datos de la fila actual
            rowData.push(cells[j].innerText.trim());
        }

        // Unir los datos de la fila con comas y agregarlos al array CSV
        csv.push(rowData.join(','));
    }

    // Unir todas las filas CSV con saltos de línea
    return csv.join('\n');
}

// Obtener la tabla por su clase
var table = document.querySelector('.data_table');

// Convertir la tabla en formato CSV
var csvData = convertToCSV(table);

// Loguear los datos CSV en la consola
console.log(csvData);
