;;; Простые горячие клавиши - обертки стандартных команд AutoCAD
;;; Команды: CT, PC, BRF, VC, VCX, rv, cc, hi, MM, RB

;;; CT - Создание окружности по 3 точкам
(DEFUN C:CT ()
  (COMMAND ".circle" "3P" )
(PRINC)
)

;;; PC - Переключение на план вида сверху
(DEFUN C:PC ()
  (command "PLAN" "C")
(PRINC)
)

;;; BRF - Разрыв линии с указанием первой точки
(DEFUN C:BRF ()
  (COMMAND ".BREAK" PAUSE "F" PAUSE (GETVAR "LASTPOINT"))
(PRINC)
)

;;; MM - Запуск VBA макроса MoveMultiplyBlocksAttributes
(DEFUN C:MM ()
  (COMMAND ".-vbarun" "MoveMultiplyBlocksAttributes")
(PRINC)
)

;;; RB - Запуск VBA макроса rotateblocks
(DEFUN C:RB ()
  (COMMAND ".-vbarun" "rotateblocks")
(PRINC)
)

;;; VC - Вставка из буфера обмена в исходные координаты
(DEFUN C:VC ()
  (COMMAND "PASTEORIG")
(PRINC)
)

;;; VCX - Вставка из буфера обмена в исходные координаты + план сверху
(DEFUN C:VCX ()
  (COMMAND "PASTEORIG" "PLAN" "C")
(PRINC)
)

;;; rv - Разворот полилинии
(DEFUN C:rv ()
  (COMMAND "REVERSE")
(PRINC)
)

;;; cc - Копирование объектов
(DEFUN C:cc ()
  (COMMAND "COPY")
(PRINC)
)

;;; hi - Запуск VBA макроса Hideshowatts (скрыть/показать атрибуты)
(DEFUN C:hi ()
  (COMMAND ".-vbarun" "Hideshowatts")
(PRINC)
)

(princ "\nHotkeys loaded: CT, PC, BRF, VC, VCX, rv, cc, hi, MM, RB")
(princ)
