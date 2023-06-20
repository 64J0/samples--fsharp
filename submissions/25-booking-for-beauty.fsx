module BookingUpForBeauty

open System

let schedule (appointmentDateDescription: string) : DateTime =
    DateTime.Parse(appointmentDateDescription)

let hasPassed (appointmentDate: DateTime) : bool = DateTime.Now > appointmentDate

let isAfternoonAppointment (appointmentDate: DateTime) : bool =
    appointmentDate.Hour > 11 && appointmentDate.Hour < 18

let description (appointmentDate: DateTime) : string =
    let parsedDate = appointmentDate.ToString("M/d/yyyy h:mm:ss tt")
    $"You have an appointment on {parsedDate}."

let anniversaryDate () : DateTime =
    DateTime(DateTime.Now.Year, 9, 15, 0, 0, 0)
