subroutine affbar(tmp, tmpf, fcx, nommai, isec,&
                  car, val, exp, ncar, kioc,&
                  ier)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/r8maem.h"
#include "asterc/r8pi.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
!
    integer :: isec, ncar, ier
    real(kind=8) :: val(*)
    character(len=6) :: kioc
    character(len=8) :: fcx, nommai, car(*), exp(*)
    character(len=24) :: tmp, tmpf
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     VERIFICATION DE LA BONNE AFFECTATION DES SECTIONS DE BARRE :
!        - INTERDICTION D ECRASER UNE GEOMETRIE DE SECTION PAR UNE AUTRE
!     AFFECTATION DES CARACTERISTIQUES GENERALES ET GEOMETRIQUES
!                         AUX MAILLES DE TYPE BARRE DANS L OBJET TAMPON
!
!     L OBJET TAMPON (TMP) CONTIENT (8*NBBARRE) VALEURS
!     EXP  : A    HY   HZ   EPY  EPZ  EP   R   TSEC
!     TSEC : TYPE  GEOMETRIQUE DE SECTION : 0 = GENERALE
!                                           1 = RECTANGLE
!                                           2 = CERCLE
!-----------------------------------------------------------------------
    real(kind=8) :: tst, pi, zero
    real(kind=8) :: hy, hz, epy, epz, hyi, hzi, e, re, ri
    character(len=8) :: resu
    character(len=24) :: valk(2)
    character(len=16) :: concep, cmd
    aster_logical :: secple
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iisec, j, jdge, jdgef, num
!-----------------------------------------------------------------------
    call jemarq()
    call getres(resu, concep, cmd)
    tst = r8maem()
    pi = r8pi()
    zero = 0.d0
    secple = .true.
!
    call jenonu(jexnom(tmp, nommai), num)
!
! --- TESTS D ECRASEMENT DE SECTION
    if (num .ne. 0) then
        call jeveuo(jexnom(tmp, nommai), 'E', jdge)
        iisec = nint(zr(jdge+ncar-1))
        if (iisec .ne. isec) then
            valk(1) = kioc
            valk(2) = nommai
            call utmess('A', 'MODELISA_69', nk=2, valk=valk)
            ier = ier + 1
            goto 9999
        endif
    else
        call jecroc(jexnom(tmp, nommai))
        call jeveuo(jexnom(tmp, nommai), 'E', jdge)
        do 5 i = 1, ncar
            zr(jdge+i-1) = tst
  5     continue
    endif
!
!     --- NOM DE LA FONCTION DU CX
    call jenonu(jexnom(tmpf, nommai), num)
    if (num .eq. 0) then
        call jecroc(jexnom(tmpf, nommai))
    endif
    call jeveuo(jexnom(tmpf, nommai), 'E', jdgef)
    zk8(jdgef) = fcx
!
    if (isec .eq. 0) then
        do 20 j = 1, ncar
            if (car(j) .eq. 'A       ') zr(jdge) = val(j)
 20     continue
    else if (isec.eq.1) then
        do 22 j = 1, ncar
            if (car(j) .eq. 'HY      ') then
                zr(jdge+1) = val(j)
            else if (car(j).eq.'HZ      ') then
                zr(jdge+2) = val(j)
            else if (car(j).eq.'EPY     ') then
                zr(jdge+3) = val(j)
                secple = .false.
            else if (car(j).eq.'EPZ     ') then
                zr(jdge+4) = val(j)
                secple = .false.
            else if (car(j).eq.'H       ') then
                zr(jdge+1) = val(j)
                zr(jdge+2) = val(j)
            else if (car(j).eq.'EP      ') then
                zr(jdge+3) = val(j)
                zr(jdge+4) = val(j)
                secple = .false.
            endif
 22     continue
    else if (isec.eq.2) then
        do 24 j = 1, ncar
            if (car(j) .eq. 'R       ') then
                zr(jdge+5) = val(j)
            else if (car(j).eq.'EP      ') then
                zr(jdge+6) = val(j)
                secple = .false.
            endif
 24     continue
    endif
    zr(jdge+7) = isec
!
! --- COMPLETUDE DES DONNES GENERALES
    if (isec .eq. 0) then
        if (zr(jdge) .eq. tst) then
            valk(1) = nommai
            valk(2) = exp(1)
            call utmess('A', 'MODELISA_70', nk=2, valk=valk)
            ier = ier + 1
        endif
        if (zr(jdge) .le. zero) then
            valk(1) = nommai
            valk(2) = exp(1)
            call utmess('A', 'MODELISA_71', nk=2, valk=valk)
            ier = ier + 1
        endif
!
! --- COMPLETUDE DES DONNES GEOMETRIQUES RECTANGLE
    else if (isec.eq.1) then
        do 40 j = 1, 2
            if (zr(jdge+j) .eq. tst) then
                valk(1) = nommai
                valk(2) = exp(1+j)
                call utmess('A', 'MODELISA_72', nk=2, valk=valk)
                ier = ier + 1
            endif
            if (zr(jdge+j) .le. zero) then
                valk(1) = nommai
                valk(2) = exp(1+j)
                call utmess('A', 'MODELISA_73', nk=2, valk=valk)
                ier = ier + 1
            endif
 40     continue
        if (.not. secple) then
            do 42 j = 3, 4
                if (zr(jdge+j) .eq. tst) then
                    valk(1) = nommai
                    valk(2) = exp(1+j)
                    call utmess('A', 'MODELISA_72', nk=2, valk=valk)
                    ier = ier + 1
                endif
                if (zr(jdge+j) .le. zero) then
                    valk(1) = nommai
                    valk(2) = exp(1+j)
                    call utmess('A', 'MODELISA_73', nk=2, valk=valk)
                    ier = ier + 1
                endif
 42         continue
        endif
!
! --- COMPLETUDE DES DONNES GEOMETRIQUES CERCLE
    else if (isec.eq.2) then
        if (zr(jdge+5) .eq. tst) then
            valk(1) = nommai
            valk(2) = exp(5)
            call utmess('A', 'MODELISA_74', nk=2, valk=valk)
            ier = ier + 1
        endif
        if (zr(jdge+5) .le. zero) then
            valk(1) = nommai
            valk(2) = exp(5)
            call utmess('A', 'MODELISA_75', nk=2, valk=valk)
            ier = ier + 1
        endif
        if (.not. secple) then
            if (zr(jdge+6) .le. zero) then
                valk(1) = nommai
                valk(2) = exp(6)
                call utmess('A', 'MODELISA_76', nk=2, valk=valk)
                ier = ier + 1
            endif
        endif
    endif
!
!
    if (ier .ne. 0) goto 9999
!
! --- AFFECTATION DES VALEURS PAR DEFAUT POUR LES DONNEES RECTANGLE
    if (isec .eq. 1) then
        hy = zr(jdge+1)
        hz = zr(jdge+2)
        if (secple) then
            zr(jdge) = hy * hz
        else
            epy = zr(jdge+3)
            epz = zr(jdge+4)
            hyi = hy - 2.d0*epy
            hzi = hz - 2.d0*epz
            zr(jdge) = hy * hz - hyi * hzi
        endif
        if (zr(jdge+3) .eq. tst) zr(jdge+3) = zr(jdge+1)
        if (zr(jdge+4) .eq. tst) zr(jdge+4) = zr(jdge+2)
!
! --- AFFECTATION DES VALEURS PAR DEFAUT POUR LES DONNEES CERCLE
    else if (isec.eq.2) then
        re = zr(jdge+5)
        if (secple) then
            zr(jdge) = pi * re * re
        else
            e = zr(jdge+6)
            ri = re - e
            zr(jdge) = pi * ( re*re - ri*ri )
        endif
        if (zr(jdge+6) .eq. tst) zr(jdge+6) = zr(jdge+5)
    endif
    do 50 i = 1, ncar
        if (zr(jdge+i-1) .eq. tst) zr(jdge+i-1) = zero
 50 end do
!
9999 continue
    call jedema()
end subroutine
