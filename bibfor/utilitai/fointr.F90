subroutine fointr(nomfon, chprol, nbvar, var, fon,&
                  nbres, varres, fonres, ier)
    implicit none
#include "jeveux.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=*) :: nomfon, chprol(*)
    integer :: nbvar, nbres, ier
    real(kind=8) :: var(*), fon(*), varres(*), fonres(*)
!     ------------------------------------------------------------------
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
!     INTERPOLATION-EXTRAPOLATION DE TOUTE UNE FONCTION
!     ------------------------------------------------------------------
! IN  NOMFON : K19 : NOM DE LA FONCTION A INTERPOLER
!            N'EST OBLIGATOIRE QUE POUR LES FONCTIONS INTERPRETEES
! IN  CHPROL : DESCRIPTEUR DES FONCTION
!           CHPROL(1) = 'FONCTION' / 'INTERPR'
!           CHPROL(2) = 'LINEAIRE' OU 'LOGARITH'
!           CHPROL(3) = NOM_PARA
!           CHPROL(4) = NOM_RESU
!           CHPROL(5) = 'GD      ' G CODE D'EXTRAPOLATION A GAUCHE
!                                  D CODE D'EXTRAPOLATION A DROITE
! IN  NBVAR  : IS : NOMBRE DE POINTS SUR LEQUEL EST DEFINIT LA FONCTION
! IN  VAR    : R8 : ABCSISSES DES POINTS DE DEFINITION DE LA FONCTION
! IN  FON    : R8 : ORDONNEES DES POINTS DE DEFINITION DE LA FONCTION
! IN  NBRES  : IS : NOMBRE DE POINTS DE DEFINITION DE L'INTERPOLEE
! IN  VARRES : R8 : ABCSISSES DES POINTS DE DEFINITION DE L'INTERPOLEE
! OUT FONRES : R8 : ORDONNEES DES POINTS DE DEFINITION DE L'INTERPOLEE
! OUT IER    : IS : CODE RETOUR
!               = 0 : O.K.
!               = 1 : ON A FAIT UNE EXTRAPOLATION A DROITE AVEC "EXCLU"
!               = 2 : ON A FAIT UNE EXTRAPOLATION A GAUCHE AVEC "EXCLU"
!               = 3 : ON A FAIT UNE EXTRAPOLATION A DROITE ET A GAUCHE
!                   : AVEC "EXCLU"
!     ------------------------------------------------------------------
!     SI EXTRAPOLATION "EXCLU" ALORS ON ARRETE EN FATAL
!     ------------------------------------------------------------------
!     SI CHPROL(1) /= 'CONSTANT'/'FONCTION'  ALORS ERREUR (AVEC ARRET)
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    character(len=19) :: nomf
    character(len=24) :: valk(3)
    real(kind=8) :: valr(3)
    integer :: i, ires, ivar, jres, lnova, lonuti
!-----------------------------------------------------------------------
!     FONCTION EN LIGNE
!
#define linlin(x,x1,y1,x2,y2) y1+(x-x1)*(y2-y1)/(x2-x1)
#define linlog(x,x1,y1,x2,y2) exp(log(y1)+(x-x1)*(log(y2)-log(y1))/(x2-x1))
#define loglog(x,x1,y1,x2,y2) exp(log(y1)+(log(x)-log(x1))*(log(y2)-log(y1))/(log(x2)-log(x1)))
#define loglin(x,x1,y1,x2,y2) y1+(log(x)-log(x1))*(y2-y1)/(log(x2)-log(x1))
!     ------------------------------------------------------------------
    call jemarq()
    ier = 0
    nomf = nomfon
!
    if (chprol(1) .eq. 'FONCTION') then
!     ------------------------------------------------------------------
!
!     INITIALISATION
!
        ivar = 1
        ires = 1
!
!     --- TRAITEMENT PARTICULIER POUR 1 POINT ---
!
        if (nbvar .eq. 1) then
            if (nbres .ne. 1 .and. chprol(5)(1:2) .ne. 'CC') then
                call utmess('F', 'FONCT0_22')
            endif
            if (chprol(5)(1:2) .eq. 'CC') then
                do 10 i = 1, nbres
                    fonres(i) = fon(ivar)
10              continue
            else
                if (varres(ires) .eq. var(ivar)) then
                    fonres(ires) = fon(ivar)
                else
                    call utmess('F', 'FONCT0_23')
                endif
            endif
            goto 9999
        endif
!
!     RECHERCHE DU DEBUT DE L'INTERVALLE D'INTERPOLATION
!
100      continue
        if ((varres(ires).lt. var(ivar)) .and. (ires.lt.nbres)) then
            ires = ires + 1
            goto 100
        endif
!
        if (ires .gt. 1) then
!
!        --- EXTRAPOLATION A GAUCHE ---
!
            if (chprol(5)(1:1) .eq. 'C') then
!           --- EXTRAPOLATION CONSTANTE ---
                do 120 jres = 1, ires-1
                    fonres(jres) = fon(ivar)
120              continue
!
            else if (chprol(5)(1:1) .eq. 'L') then
!           --- EXTRAPOLATION LINEAIRE ---
                do 130 jres = 1, ires-1
                    fonres(jres)=linlin(varres(jres),var(ivar),fon(ivar), var(ivar+1),fon(ivar+1))
130              continue
!
            else if (chprol(5)(1:1) .eq. 'I') then
                call jeveuo(nomf//'.NOVA', 'L', lnova)
                do 140 jres = 1, nbres
                    call fointe('F ', nomf, 1, zk8(lnova), varres(jres),&
                                fonres(jres), ier)
140              continue
!
            else if (chprol(5)(1:1) .eq. 'E') then
!           --- EXTRAPOLATION EXCLUE ---
                ier = ier + 1
                valr(1)=varres(1)
                valr(2)=var(1)
                call utmess('F+', 'FONCT0_9', sk=nomf)
                call utmess('F', 'FONCT0_19', nr=2, valr=valr)
            else
                call utmess('F', 'FONCT0_21', sk=chprol(5)(1:1))
            endif
        endif
!
!     --- INTERPOLATION ---
!
200      continue
        if (ires .le. nbres) then
210          continue
            if (varres(ires) .le. var(ivar+1)) then
                if (chprol(2)(1:8) .eq. 'LIN LIN ') then
!              --- INTERPOLATION LINEAIRE ---
                    fonres(ires) = &
                        linlin(varres(ires), var(ivar), fon(ivar), var(ivar+1), fon(ivar+1))
                else if (chprol(2)(1:8).eq.'LOG LOG ') then
!              --- INTERPOLATION LOGARITHMIQUE ---
                    fonres(ires) = &
                        loglog(varres(ires), var(ivar), fon(ivar), var(ivar+1), fon(ivar+1))
                else if (chprol(2)(1:8).eq.'LIN LOG ') then
!              --- INTERPOLATION LIN-LOG ---
                    fonres(ires) = &
                        linlog(varres(ires), var(ivar), fon(ivar), var(ivar+1), fon(ivar+1))
                else if (chprol(2)(1:8).eq.'LOG LIN ') then
!              --- INTERPOLATION LOG-LIN ---
                    fonres(ires) = &
                        loglin(varres(ires), var(ivar), fon(ivar), var(ivar+1), fon(ivar+1))
                else if (chprol(2)(1:3).eq.'INT') then
                    call jeveuo(nomf//'.NOVA', 'L', lnova)
                    call fointe('F ', nomf, 1, zk8(lnova), varres(ires),&
                                fonres(ires), ier)
                else if (chprol(2)(1:3).eq.'NON') then
                    if (varres(ires) .eq. var(ivar)) then
                        fonres(ires) = fon(ivar)
                    else
                        if (varres(ires) .eq. var(ivar+1)) then
                            fonres(ires) = fon(ivar+1)
                        else
                            ier = ier + 1
                            valr (1) = varres(ires)
                            valr (2) = var(ivar)
                            valr (3) = var(ivar+1)
                            call utmess('F+', 'FONCT0_11', sk=nomf)
                            call utmess('F', 'FONCT0_26', nr=3, valr=valr)
                        endif
                    endif
                else
                    ier = ier + 1
                    call utmess('F', 'FONCT0_21', sk=chprol(2))
                endif
                ires = ires + 1
                goto 200
            else
                ivar = ivar + 1
                if (ivar .lt. nbvar) goto 210
            endif
        endif
!
!
        if (ires .lt. nbres) then
!
!        --- EXTRAPOLATION A DROITE ---
!
            if (chprol(5)(2:2) .eq. 'C') then
!           --- EXTRAPOLATION CONSTANTE ---
                do 310 jres = ires, nbres
                    fonres(jres) = fon(nbvar)
310              continue
            else if (chprol(5)(2:2) .eq. 'L') then
                do 320 jres = ires, nbres
                    fonres(jres) = &
                        linlin(varres(jres),var(nbvar-1),fon(nbvar-1), var(nbvar),fon(nbvar))
320              continue
            else if (chprol(5)(2:2) .eq. 'I') then
!           --- EXTRAPOLATION INTERPRETEE ----
                call jeveuo(nomf//'.NOVA', 'L', lnova)
                do 330 jres = ires, nbres
                    call fointe('F ', nomf, 1, zk8(lnova), varres(jres),&
                                fonres(jres), ier)
330              continue
!
            else if (chprol(5)(2:2) .eq. 'E') then
!           --- EXTRAPOLATION EXCLUE ---
                ier = ier + 2
                valr(1)=varres(nbres)
                valr(2)=var(nbvar)
                call utmess('F+', 'FONCT0_9', sk=nomf)
                call utmess('F', 'FONCT0_20', nr=2, valr=valr)
            else
                call utmess('F', 'FONCT0_21', sk=chprol(5)(2:2))
            endif
        endif
        goto 9999
!     ------------------------------------------------------------------
    else if (chprol(1) .eq.'CONSTANT') then
        do 1100 jres = 1, nbres
            fonres(jres) = fon(1)
1100      continue
    else if (chprol(1) .eq.'INTERPRE') then
        call jelira(nomf//'.NOVA', 'LONUTI', lonuti)
        if (lonuti .ne. 1) then
            call utmess('F', 'FONCT0_24', sk=nomf, si=lonuti)
        endif
        call jeveuo(nomf//'.NOVA', 'L', lnova)
        do 1200 jres = 1, nbres
            call fointe('F ', nomf, 1, zk8(lnova), varres(jres),&
                        fonres( jres), ier)
1200      continue
    else
        valk(1)=nomf
        valk(2)=chprol(1)
        valk(3)='FOINTR'
        call utmess('F', 'FONCT0_25', nk=3, valk=valk)
    endif
9999  continue
    call jedema()
end subroutine
