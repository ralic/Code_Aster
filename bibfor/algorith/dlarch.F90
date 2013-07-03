subroutine dlarch(result, neq, istoc, iarchi, texte,&
                  alarm, ifm, temps, nbtyar, typear,&
                  masse, depl, vite, acce, fexte,&
                  famor, fliai)
! ---------------------------------------------------------------------
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
!
!  ARCHIVAGE DANS L'OBJET CHAMNO DU CHAMP DE DEPLACEMENT,DE VITESSE
!  ET/OU D'ACCELERATION ISSU D'UN CALCUL TRANSITOIRE DIRECT
!
! ---------------------------------------------------------------------
!  IN  : NEQ       : NOMBRE D'EQUATIONS
!  IN  : ISTOC     : PILOTAGE DU STOCKAGE DES RESULTATS
!  IN  : IARCHI    : PILOTAGE DE L'ARCHIVAGE DES RESULTATS
!  IN  : TEXTE     : COMMENTAIRE A IMPRIMER
!  IN  : ALARM     : EMISSION D'ALARME SI >0
!  IN  : TEMPS     : INSTANT D'ARCHIVAGE
!  IN  : NBTYAR    : TAILLE DE TYPEAR
!  IN  : TYPEAR    : TABLEAU INDIQUANT SI ON ARCHIVE LES DIFFERENTS
!                    CHAMPS (DEPL, VIT ET ACC) (NBTYAR)
!  IN  : MASSE     : NOM DE LA MATRICE DE MASSE
!  IN  : DEPL      : TABLEAU DES DEPLACEMENTS
!  IN  : VITE      : TABLEAU DES VITESSES
!  IN  : ACCE      : TABLEAU DES ACCELERATIONS
!
!     ------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit   none
! DECLARATION PARAMETRES D'APPELS
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jelibe.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/u2mesk.h"
#include "asterfort/vtcrem.h"
    integer :: neq, istoc, iarchi, alarm, ifm
    integer :: nbtyar
!
    real(kind=8) :: depl(neq), vite(neq), acce(neq)
    real(kind=8) :: fexte(neq), famor(neq), fliai(neq)
    real(kind=8) :: temps
!
    character(len=8) :: masse
    character(len=8) :: result
    character(len=16) :: typear(nbtyar)
    character(len=*) :: texte
!
!
!
    integer :: iaux, jaux, itype
    integer :: lgcomm
    character(len=8) :: k8b
!
    character(len=24) :: chamno
!
!====
! 1. PREALABLES
!====
!
!
! 1.2. ==> INSTANT
!
    if (istoc .eq. 0) then
        iarchi = iarchi + 1
        call rsadpa(result, 'E', 1, 'INST', iarchi,&
                    0, iaux, k8b)
        zr(iaux) = temps
    else
        call rsadpa(result, 'L', 1, 'INST', iarchi,&
                    0, iaux, k8b)
        temps = zr(iaux)
    endif
!
! 1.3. ==> COMMENTAIRE
!
    lgcomm = lxlgut(texte)
!
!====
! 2. STOCKAGE DES CHAMPS DESIGNES
!====
!
    do 21 , itype = 1, nbtyar
!
    if (typear(itype) .ne. '    ') then
!
        call rsexch(' ', result, typear(itype), iarchi, chamno,&
                    iaux)
        if (iaux .eq. 0) then
            if (alarm .gt. 0) then
                call u2mesk('A', 'ALGORITH2_64', 1, chamno)
            endif
            goto 21
        else if (iaux.eq.100) then
            call vtcrem(chamno, masse, 'G', 'R')
        else
            call assert(.false.)
        endif
!
        chamno(20:24) = '.VALE'
        call jeveuo(chamno, 'E', jaux)
!
        if (typear(itype) .eq. 'DEPL') then
            do 211 , iaux = 1, neq
            zr(jaux+iaux-1) = depl(iaux)
211          continue
        else if (typear(itype).eq.'VITE') then
            do 212 , iaux = 1, neq
            zr(jaux+iaux-1) = vite(iaux)
212          continue
        else if (typear(itype).eq.'ACCE') then
            do 213 , iaux = 1, neq
            zr(jaux+iaux-1) = acce(iaux)
213          continue
        else if (typear(itype).eq.'FORC_EXTE') then
            do 214 , iaux = 1, neq
            zr(jaux+iaux-1) = fexte(iaux)
214          continue
        else if (typear(itype).eq.'FORC_AMOR') then
            do 215 , iaux = 1, neq
            zr(jaux+iaux-1) = famor(iaux)
215          continue
        else if (typear(itype).eq.'FORC_LIAI') then
            do 216 , iaux = 1, neq
            zr(jaux+iaux-1) = fliai(iaux)
216          continue
        endif
!
        call jelibe(chamno)
        call rsnoch(result, typear(itype), iarchi)
!
    endif
!
    21 end do
!
    istoc = 1
!
    if (lgcomm .eq. 0) then
        write(ifm,2000) (typear(iaux),iaux=1,nbtyar), iarchi, temps
    else
        write(ifm,2001) texte(1:lgcomm), (typear(iaux),iaux=1,nbtyar),&
        iarchi, temps
    endif
    2000 format(1p,3x,'CHAMP(S) STOCKE(S):',3(1x,a4),3(1x,a9),&
     &             ' NUME_ORDRE:',i8,' INSTANT:',d12.5)
    2001 format(1p,3x,a,1x,'CHAMP(S) STOCKE(S):',3(1x,a4),3(1x,a9),&
     &             ' NUME_ORDRE:',i8,' INSTANT:',d12.5)
!
end subroutine
