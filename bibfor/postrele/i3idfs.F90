subroutine i3idfs(epsi, k, f, nba, sgt,&
                  coorsm, nbpt, lstpt, fink)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/i3crad.h"
#include "asterfort/i3ptrv.h"
#include "asterfort/i3sl32.h"
#include "asterfort/utmess.h"
    integer :: k, f, nbpt, lstpt(*), nba
    real(kind=8) :: epsi, sgt(*), coorsm(3, *)
    aster_logical :: fink
!
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
!     ------------------------------------------------------------------
!     INTERSECTION FRONTIERE DE FACE ET SEGMENT COPLANAIRE
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! IN  F      : I : F CONSIDEREE
! IN  NBA    : I : NOMBRE D' ARETE SUR LA FACE
! IN  K      : I : -
! IN  COORSM : R : TABLE DES COORDONEES DES SOMMETS (ORDRE DE LA CONNEC)
! IN  SGT    : R : COORDONNEES DES POINTS A ET B -
! VAR FINK   : L : INDICATEUR DE FIN DE REPERAGE NIVEAU K
! OUT NBPT   : I : NOMBRE DE POINT TROUVE
!            :   : CONVENTION NBPT = -2 <=> CARD(INTER) = INFINI
!            :   : DANS CE CAS OUT = EXTREMITES
! OUT LSTPT  : I : OBJ LISTE_POINT
!     ------------------------------------------------------------------
!
!
!
    character(len=4) :: typsl
    integer :: arete, nd, nf, i, ipos
    real(kind=8) :: a(3, 2), x(2), b(3), t, td, tf, r1, r2, r3, t1, t2, nrmab
    real(kind=8) :: zero, un
    aster_logical :: finf, dejala
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    arete = 0
    zero = 0.0d0
    un = 1.0d0
    finf = .false.
    nbpt = 0
100 continue
    if (.not. finf) then
        arete = arete + 1
        nd = arete
        nf = max(1,mod(nd+1,nba+1))
        nrmab = zero
        do 120 i = 1, 3, 1
            r1 = coorsm(i,nd)
            r2 = coorsm(i,nf)
            a(i,1) = r2 - r1
            b(i) = -r1
            nrmab = nrmab + (sgt(i+3)-sgt(i))*(sgt(i+3)-sgt(i))
120     continue
        nrmab = sqrt(nrmab)
        a(1,2) = zero
        a(2,2) = zero
        a(3,2) = -nrmab
        do 130 i = 1, 3, 1
            t = max(abs(a(i,1)),abs(a(i,2)))
            if (abs(t) .gt. epsi) then
                t = un/t
                a(i,1) = a(i,1)*t
                a(i,2) = a(i,2)*t
                b(i) = b(i)*t
            endif
130     continue
        call i3sl32(epsi, a, b, x, typsl)
        if (typsl .eq. 'INCO') then
!           ARETE ET SGT // DISTINCT  ==> INTER = VIDE
!           DONC ACTION = VIDE
        else if (typsl .eq. 'DETE') then
            t = x(2)
            r3 = x(1)
            if (abs(t) .le. epsi) then
                t = zero
            endif
            if (abs(t-un) .le. epsi) then
                t = un
            endif
            if (abs(r3-un) .le. epsi) then
                r3 = un
            endif
            if (abs(r3) .le. epsi) then
                r3 = zero
            endif
            call i3ptrv(epsi, lstpt, nbpt, t, dejala,&
                        ipos)
            if ((r3 .ge. zero) .and. (r3.le. (un+epsi)) .and. (t .ge. zero) .and.&
                (t .le. (un+epsi)) .and. (.not. dejala)) then
                call i3crad(k, f, arete, nba, r3,&
                            r1, r2)
                zr(lstpt(1) + nbpt) = t
                zi(lstpt(2) + nbpt) = f
                zi(lstpt(3) + nbpt) = arete
                zi(lstpt(4) + nbpt) = 0
                zr(lstpt(5) + 2*nbpt+1-1) = r1
                zr(lstpt(5) + 2*nbpt+2-1) = r2
                zi(lstpt(6) + nbpt) = nbpt + 1
                nbpt = nbpt + 1
            endif
        else if (typsl .eq. 'INDE') then
!     /* PASSAGE AU NIVEAU INFERRIEUR              */
!     /* SGT INTER FACE INCLUS DANS FRONTIERE FACE */
            nbpt = 0
            fink = .true.
            finf = .true.
            td = coorsm(3,nd)/nrmab
            tf = coorsm(3,nf)/nrmab
            t1 = max(zero,min(td,tf))
            t2 = min(un,max(td,tf))
            if (abs(t1-t2) .le. epsi) then
                t = 0.5d0*(t1 + t2)
                r3 = (t-td)/(tf-td)
                call i3crad(k, f, arete, nba, r3,&
                            r1, r2)
                zr(lstpt(1) ) = t
                zi(lstpt(2) ) = f
                zi(lstpt(3) ) = arete
                zi(lstpt(4) ) = 0
                zr(lstpt(5) ) = r1
                zr(lstpt(5)+1) = r2
                zi(lstpt(6) ) = 1
                nbpt = -1
            else if (t1 .lt. t2) then
                t = t1
                r3 = (t-td)/(tf-td)
                call i3crad(k, f, arete, nba, r3,&
                            r1, r2)
                zr(lstpt(1) + nbpt) = t
                zi(lstpt(2) + nbpt) = f
                zi(lstpt(3) + nbpt) = arete
                zi(lstpt(4) + nbpt) = 0
                zr(lstpt(5) + 2*nbpt+1-1) = r1
                zr(lstpt(5) + 2*nbpt+2-1) = r2
                zi(lstpt(6) + nbpt) = nbpt + 1
                nbpt = nbpt + 1
                t = t2
                r3 = (t-td)/(tf-td)
                call i3crad(k, f, arete, nba, r3,&
                            r1, r2)
                zr(lstpt(1) + nbpt) = t
                zi(lstpt(2) + nbpt) = f
                zi(lstpt(3) + nbpt) = arete
                zi(lstpt(4) + nbpt) = 0
                zr(lstpt(5) + 2*nbpt+1-1) = r1
                zr(lstpt(5) + 2*nbpt+2-1) = r2
                zi(lstpt(6) + nbpt) = nbpt + 1
                nbpt = nbpt + 1
                r1 = zr(lstpt(1))
                r2 = zr(lstpt(1)+1)
            else
            endif
            nbpt = -nbpt
        else
            call utmess('F', 'INTEMAIL_8', sk=typsl)
        endif
        finf = ( finf .or. (arete .ge. nba) )
        goto 100
    endif
end subroutine
