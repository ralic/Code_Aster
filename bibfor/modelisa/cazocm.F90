subroutine cazocm(char, motfac, izone)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/normev.h"
#include "asterfort/u2mess.h"
    character(len=8) :: char
    character(len=16) :: motfac
    integer :: izone
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE MAILLEE - LECTURE DONNEES)
!
! LECTURE DES PRINCIPALES CARACTERISTIQUES DU CONTACT
! CONCERNANT L'APPARIEMENT ET SES OPTIONS
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  IFORM  : TYPE DE FORMULATION
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
! IN  IZONE  : INDICE POUR LIRE LES DONNEES
!
!
!
!
!
    integer :: zmeth, zdirn, ztole
    integer :: noc
    character(len=24) :: defico
    character(len=24) :: methco, toleco
    integer :: jmeth, jtole
    character(len=24) :: dirapp, dirnor
    integer :: jdirap, jdirno
    character(len=24) :: jeufo1, jeufo2
    integer :: jjfo1, jjfo2
    character(len=16) :: appa, norm, apty, typnor, typm, veri
    real(kind=8) :: noor
    real(kind=8) :: dir(3), tolj, tola, tolint
    character(len=8) :: jeuf1, jeuf2
    logical :: lliss, lcalc
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    tolj = 0.d0
    tola = 0.d0
    jeuf1 = ' '
    jeuf2 = ' '
    dir(1) = 0.d0
    dir(2) = 0.d0
    dir(3) = 0.d0
    tolint = 0.d0
    lcalc = .true.
    lliss = cfdisl(defico,'LISSAGE')
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    methco = defico(1:16)//'.METHCO'
    toleco = defico(1:16)//'.TOLECO'
    dirapp = defico(1:16)//'.DIRAPP'
    dirnor = defico(1:16)//'.DIRNOR'
    jeufo1 = defico(1:16)//'.JFO1CO'
    jeufo2 = defico(1:16)//'.JFO2CO'
!
    call jeveuo(dirapp, 'E', jdirap)
    call jeveuo(dirnor, 'E', jdirno)
    call jeveuo(methco, 'E', jmeth)
    call jeveuo(toleco, 'E', jtole)
    call jeveuo(jeufo1, 'E', jjfo1)
    call jeveuo(jeufo2, 'E', jjfo2)
!
    zmeth = cfmmvd('ZMETH')
    zdirn = cfmmvd('ZDIRN')
    ztole = cfmmvd('ZTOLE')
!
! --- RECUPERATION DU TYPE D'APPARIEMENT
!
    call getvtx(motfac, 'APPARIEMENT', izone, iarg, 1,&
                appa, noc)
    if (appa(1:5) .eq. 'NODAL') then
        zi(jmeth+zmeth*(izone-1)+1-1) = 0
    else if (appa(1:9) .eq. 'MAIT_ESCL') then
        zi(jmeth+zmeth*(izone-1)+1-1) = 1
    else
        call assert(.false.)
    endif
!
! --- PRESENCE DE DIST_POUTRE/DIST_COQUE
!
    call getvtx(motfac, 'DIST_POUTRE', izone, iarg, 1,&
                typm, noc)
    if (typm(1:3) .eq. 'OUI') then
        zi(jmeth+zmeth*(izone-1)+2-1) = 1
    endif
    call getvtx(motfac, 'DIST_COQUE', izone, iarg, 1,&
                typm, noc)
    if (typm(1:3) .eq. 'OUI') then
        zi(jmeth+zmeth*(izone-1)+3-1) = 1
    endif
!
! --- TYPE DE NORMALE
!
    call getvtx(motfac, 'NORMALE', izone, iarg, 1,&
                norm, noc)
!
    if (norm(1:4) .eq. 'MAIT') then
        if (norm(5:9) .eq. '_ESCL') then
            zi(jmeth+zmeth*(izone-1)+4-1) = 1
        else
            zi(jmeth+zmeth*(izone-1)+4-1) = 0
        endif
    else if (norm(1:4) .eq. 'ESCL') then
        zi(jmeth+zmeth*(izone-1)+4-1) = 2
    else
        call assert(.false.)
    endif
!
! --- DEFINITION DU TYPE DE NORMALE - MAITRE
!
    call getvtx(motfac, 'VECT_MAIT', izone, iarg, 1,&
                typnor, noc)
!
    if (typnor .eq. 'AUTO') then
        zi(jmeth+zmeth*(izone-1)+5-1) = 0
    else if (typnor.eq.'FIXE') then
        if (zi(jmeth+zmeth*(izone-1)+4-1) .ne. 0) then
            call u2mess('F', 'CONTACT3_50')
        endif
        if (lliss) then
            call u2mess('F', 'CONTACT3_54')
        endif
        zi(jmeth+zmeth*(izone-1)+5-1) = 1
        call getvr8(motfac, 'MAIT_FIXE', izone, iarg, 3,&
                    dir, noc)
        if (noc .eq. 0) then
            call assert(.false.)
        endif
        call normev(dir, noor)
        if (noor .le. r8prem()) then
            call u2mess('F', 'CONTACT_15')
        endif
        zr(jdirno+zdirn*(izone-1)) = dir(1)
        zr(jdirno+zdirn*(izone-1)+1) = dir(2)
        zr(jdirno+zdirn*(izone-1)+2) = dir(3)
    else if (typnor.eq.'VECT_Y') then
        if (zi(jmeth+zmeth*(izone-1)+4-1) .ne. 0) then
            call u2mess('F', 'CONTACT3_51')
        endif
        if (lliss) then
            call u2mess('F', 'CONTACT3_54')
        endif
        zi(jmeth+zmeth*(izone-1)+5-1) = 2
        call getvr8(motfac, 'MAIT_VECT_Y', izone, iarg, 3,&
                    dir, noc)
        if (noc .eq. 0) then
            call assert(.false.)
        endif
        call normev(dir, noor)
        if (noor .le. r8prem()) then
            call u2mess('F', 'CONTACT_16')
        endif
        zr(jdirno+zdirn*(izone-1)) = dir(1)
        zr(jdirno+zdirn*(izone-1)+1) = dir(2)
        zr(jdirno+zdirn*(izone-1)+2) = dir(3)
    else
        call assert(.false.)
    endif
!
! --- DEFINITION DU TYPE DE NORMALE - ESCLAVE
!
    call getvtx(motfac, 'VECT_ESCL', izone, iarg, 1,&
                typnor, noc)
!
    if (typnor .eq. 'AUTO') then
        zi(jmeth+zmeth*(izone-1)+6-1) = 0
    else if (typnor.eq.'FIXE') then
        if (zi(jmeth+zmeth*(izone-1)+4-1) .ne. 2) then
            call u2mess('F', 'CONTACT3_52')
        endif
        if (lliss) then
            call u2mess('F', 'CONTACT3_54')
        endif
        zi(jmeth+zmeth*(izone-1)+6-1) = 1
        call getvr8(motfac, 'ESCL_FIXE', izone, iarg, 3,&
                    dir, noc)
        if (noc .eq. 0) then
            call assert(.false.)
        endif
        call normev(dir, noor)
        if (noor .le. r8prem()) then
            call u2mess('F', 'CONTACT_15')
        endif
        zr(jdirno+zdirn*(izone-1)+3) = dir(1)
        zr(jdirno+zdirn*(izone-1)+4) = dir(2)
        zr(jdirno+zdirn*(izone-1)+5) = dir(3)
    else if (typnor.eq.'VECT_Y') then
        if (zi(jmeth+zmeth*(izone-1)+4-1) .ne. 2) then
            call u2mess('F', 'CONTACT3_53')
        endif
        if (lliss) then
            call u2mess('F', 'CONTACT3_54')
        endif
        zi(jmeth+zmeth*(izone-1)+6-1) = 2
        call getvr8(motfac, 'ESCL_VECT_Y', izone, iarg, 3,&
                    dir, noc)
        if (noc .eq. 0) then
            call assert(.false.)
        endif
        call normev(dir, noor)
        if (noor .le. r8prem()) then
            call u2mess('F', 'CONTACT_16')
        endif
        zr(jdirno+zdirn*(izone-1)+3) = dir(1)
        zr(jdirno+zdirn*(izone-1)+4) = dir(2)
        zr(jdirno+zdirn*(izone-1)+5) = dir(3)
    else
        call assert(.false.)
    endif
!
! --- RECH. APPARIEMENT DANS UNE DIRECTION FIXE: DIRE_APPA
!
    call getvtx(motfac, 'TYPE_APPA', izone, iarg, 1,&
                apty, noc)
    if (apty(1:6) .eq. 'PROCHE') then
        zi(jmeth+zmeth*(izone-1)+7-1) = 0
    else if (apty(1:4) .eq. 'FIXE') then
        zi(jmeth+zmeth*(izone-1)+7-1) = 1
        call getvr8(motfac, 'DIRE_APPA', izone, iarg, 3,&
                    dir, noc)
        call normev(dir, noor)
        if (noor .le. r8prem()) then
            call u2mess('F', 'CONTACT3_15')
        endif
        zr(jdirap+3*(izone-1)) = dir(1)
        zr(jdirap+3*(izone-1)+1) = dir(2)
        zr(jdirap+3*(izone-1)+2) = dir(3)
    else
        call assert(.false.)
    endif
!
! --- PRESENCE DE VERIF
!
    call getvtx(motfac, 'RESOLUTION', izone, iarg, 1,&
                veri, noc)
    if (veri(1:3) .eq. 'NON') then
        zi(jmeth+zmeth*(izone-1)+22-1) = 1
        lcalc = .false.
    endif
!
! --- PARAMETRE APPARIEMENT: DIST_MAIT/DIST_ESCL
!
    call getvid(motfac, 'DIST_MAIT', izone, iarg, 1,&
                jeuf1, noc)
    if (noc .ne. 0) zk8(jjfo1+izone-1) = jeuf1
    call getvid(motfac, 'DIST_ESCL', izone, iarg, 1,&
                jeuf2, noc)
    if (noc .ne. 0) zk8(jjfo2+izone-1) = jeuf2
!
! --- PARAMETRE APPARIEMENT: TOLE_PROJ_EXT
! --- TOLE_PROJ_EXT <0: LA PROJECTION HORS DE LA MAILLE EST INTERDITE
! --- TOLE_PROJ_EXT >0: LA PROJECTION HORS DE LA MAILLE EST AUTORISEE
! ---                    MAIS LIMITEE PAR TOLJ
!
    call getvr8(motfac, 'TOLE_PROJ_EXT', izone, iarg, 1,&
                tolj, noc)
    if (tolj .lt. 0.d0) then
        zr(jtole+ztole*(izone-1)+1-1) = -1.d0
    else
        zr(jtole+ztole*(izone-1)+1-1) = tolj
    endif
!
! --- PARAMETRE APPARIEMENT: TOLE_APPA
! --- TOLE_APPA <0: ON APPARIE QUELQUE SOIT LA DISTANCE ESCLAVE/MAITRE
! --- TOLE_APPA >0: ON APPARIE SI DISTANCE ESCLAVE/MAITRE<TOLE
!
    call getvr8(motfac, 'TOLE_APPA', izone, iarg, 1,&
                tola, noc)
    if (tola .lt. 0.d0) then
        zr(jtole+ztole*(izone-1)+2-1) = -1.d0
    else
        zr(jtole+ztole*(izone-1)+2-1) = tola
    endif
!
! --- SPECIFIQUE METHODE SANS RESOLUTION
!
    if (.not.lcalc) then
        call getvr8(motfac, 'TOLE_INTERP', izone, iarg, 1,&
                    tolint, noc)
        zr(jtole+ztole*(izone-1)+3-1) = tolint
    endif
!
    call jedema()
end subroutine
