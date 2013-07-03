subroutine affpou(tmp, tmpf, fcx, nom, isec,&
                  ivar, car, ncar, val, tab,&
                  exp, nbo, ioc, ier)
    implicit none
#include "jeveux.h"
!
#include "asterc/r8maem.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
    real(kind=8) :: val(*)
    character(len=6) :: ioc
    character(len=8) :: fcx, car(*), tab(*), exp(*)
    character(len=24) :: tmp, tmpf, nom
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     VERIFICATION DE LA BONNE AFFECTATION DES SECTIONS DE POUTRE :
!        - INTERDICTION D ECRASER UNE GEOMETRIE DE SECTION PAR UNE AUTRE
!        - INTERDICTION D ECRASER UNE VARIATION DE SECTION PAR UNE AUTRE
!          AFFECTATION DES CARACTERISTIQUES GENERALES ET GEOMETRIQUES
!          AUX MAILLES DE TYPE POUTRE DANS L OBJET TAMPON
!     ------------------------------------------------------------------
!     L'OBJET TAMPON CONTIENT (44*NBPOUTR) VALEURS
!     ------------------------------------------------------------------
!        TAB  1     2     3     4     5    6    7    8    9    10
!        0    A1    IY1   IZ1   AY1   AZ1  EY1  EZ1  JX1  RY1  RZ1
!        1    RT1   A2    IY2   IZ2   AY2  AZ2  EY2  EZ2  JX2  RY2
!        2    RZ2   RT2   TVAR  HY1   HZ1  EPY1 EPZ1 HY2  HZ2  EPY2
!        3    EPZ2  R1    E1    R2    E2   TSEC AI1  AI2  JG1  JG2
!        4    IYR21 IYR22 IZR21 IZR22
!     ------------------------------------------------------------------
!        TSEC = TYPE  GEOMETRIQUE DE SECTION : 0 = GENERALE
!                                              1 = RECTANGLE
!                                              2 = CERCLE
!        TVAR = TYPE DE VARIATION DE SECTION : 0 = CONSTANTE
!                                             (1 = AFFINITE)
!                                              2 = HOMOTHETIE
!     ------------------------------------------------------------------
    character(len=24) :: valk(2)
    character(len=8) :: valkm
    integer :: valim
    real(kind=8) :: tst, valrm
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ier, iisec, iivar, isec, ivar, j
    integer :: jdge, jdgef, nbo, ncar, num
!-----------------------------------------------------------------------
    call jemarq()
    tst = r8maem()
!
    call jenonu(jexnom(tmp, nom), num)
!
!     --- TESTS D'ECRASEMENT DE SECTION ---
    if (num .ne. 0) then
        call jeveuo(jexnom(tmp, nom), 'E', jdge)
        iivar = nint(zr(jdge+22))
        iisec = nint(zr(jdge+35))
        if (iivar .ne. ivar) then
            valk(1) = ioc
            valk(2) = nom
            call u2mesk('A', 'MODELISA_92', 2, valk)
            ier = ier + 1
        endif
        if (iisec .ne. isec) then
            valk(1) = ioc
            valk(2) = nom
            call u2mesk('A', 'MODELISA_93', 2, valk)
            ier = ier + 1
        endif
!
    else
        call jecroc(jexnom(tmp, nom))
        call jeveuo(jexnom(tmp, nom), 'E', jdge)
        do 5 i = 1, nbo
            zr(jdge+i-1) = tst
 5      continue
    endif
!
! --- VERIFICATION QUE LES AY, AZ SONT >= 1
    do 7 i = 1, ncar
        if ((car(i)(1:2).eq.'AY') .or. (car(i)(1:2).eq.'AZ')) then
            if (val(i) .lt. 1.0d0) then
                valkm = car(i)
                valrm = val(i)
                call u2mesg('A', 'MODELISA_23', 1, valkm, 0,&
                            valim, 1, valrm)
            endif
        endif
 7  end do
!
!     --- NOM DE LA FONCTION DU CX
    call jenonu(jexnom(tmpf, nom), num)
    if (num .eq. 0) then
        call jecroc(jexnom(tmpf, nom))
    endif
    call jeveuo(jexnom(tmpf, nom), 'E', jdgef)
    zk8(jdgef) = fcx
!
!     --- VALEURS AUX EXTREMITES POUR SECTION VARIABLE
    do 10 i = 1, ncar
        do 20 j = 1, nbo
            if (car(i) .eq. tab(j)) zr(jdge+j-1) = val(i)
20      continue
10  end do
!
!     --- EXPANSION DES VALEURS AUX EXTREMITES POUR SECTION CONSTANTE
    do 30 i = 1, ncar
        do 40 j = 1, nbo
            if (car(i) .eq. exp(j)) zr(jdge+j-1) = val(i)
40      continue
30  end do
!
!     --- EXPANSION DANS LE CAS PARTICULIER DE LA SECTION CARRE (H,EP)
    do 50 i = 1, ncar
        if (car(i) .eq. 'H1      ' .or. car(i) .eq. 'H       ') then
            zr(jdge+23) = val(i)
            zr(jdge+24) = val(i)
        endif
        if (car(i) .eq. 'H2      ' .or. car(i) .eq. 'H       ') then
            zr(jdge+27) = val(i)
            zr(jdge+28) = val(i)
        endif
        if (car(i) .eq. 'EP1     ' .or. car(i) .eq. 'EP      ') then
            zr(jdge+25) = val(i)
            zr(jdge+26) = val(i)
        endif
        if (car(i) .eq. 'EP2     ' .or. car(i) .eq. 'EP      ') then
            zr(jdge+29) = val(i)
            zr(jdge+30) = val(i)
        endif
50  end do
!
!     --- TYPE/GEOMETRIE  DE SECTION ---
    zr(jdge+22) = ivar
    zr(jdge+35) = isec
!
    call jedema()
end subroutine
