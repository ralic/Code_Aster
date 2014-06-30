subroutine nmchcc(fonact, nbmatr, ltypma, loptme, loptma,&
                  lassme, lcalme)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterfort/isfonc.h"
#include "asterfort/nmcmat.h"
    integer :: fonact(*)
    integer :: nbmatr
    character(len=6) :: ltypma(20)
    character(len=16) :: loptme(20), loptma(20)
    logical(kind=1) :: lassme(20), lcalme(20)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! MATR_ELEM CONTACT/XFEM
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES
! I/O NBMATR : NOMBRE DE MATR_ELEM DANS LA LISTE
! I/O LTYPMA : LISTE DES NOMS DES MATR_ELEM
! I/O LOPTME : LISTE DES OPTIONS DES MATR_ELEM
! I/O LOPTMA : LISTE DES OPTIONS DES MATR_ASSE
! I/O LCALME : SI MATR_ELEM A CALCULER
! I/O LASSME : SI MATR_ELEM A ASSEMBLER
!
! ----------------------------------------------------------------------
!
    logical(kind=1) :: leltc, leltf
!
! ----------------------------------------------------------------------
!
!
! --- FONCTIONNALITES ACTIVEES
!
    leltc = isfonc(fonact,'ELT_CONTACT')
    leltf = isfonc(fonact,'ELT_FROTTEMENT')
!
! --- ELEMENTS DE CONTACT (XFEM+CONTINU)
!
    if (leltc) then
        call nmcmat('AJOU', 'MEELTC', ' ', ' ', .true._1,&
                    .false._1, nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
    endif
!
! --- ELEMENTS DE FROTTEMENT (XFEM+CONTINU)
!
    if (leltf) then
        call nmcmat('AJOU', 'MEELTF', ' ', ' ', .true._1,&
                    .false._1, nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
    endif
!
end subroutine
