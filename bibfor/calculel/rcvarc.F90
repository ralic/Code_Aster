subroutine rcvarc(arret, novrc, poum, fami, kpg,&
                  ksp, valvrc, iret)
    implicit none
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
#include "jeveux.h"
#include "asterc/iisnan.h"
#include "asterc/indik8.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/rcvarp.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"

    character(len=*), intent(in) :: novrc, poum, fami
    character(len=1), intent(in) :: arret
    integer, intent(out) :: iret
    integer, intent(in) :: kpg, ksp
    real(kind=8), intent(out) :: valvrc
!-----------------------------------------------------------------------
! BUT: RECUPERER LA VALEUR D'UNE VARIABLE DE COMMANDE SUR UN SOUS-POINT
!      DE GAUSS (KPG,KSP) ET POUR UNE VALEUR D'INSTANT ('+','-','REF')

! ARGUMENTS :
!  IN   ARRET (K1)  : CE QU'IL FAUT FAIRE EN CAS DE PROBLEME
!              = ' ' : ON REMPLIT CODRET ET ON SORT SANS MESSAGE.
!              = 'F' : SI LA VARIABLE N'EST PAS TROUVEE, ON ARRETE
!                       EN FATAL.
!  IN   NOVRC  (K8) : NOM DE LA VARIABLE DE COMMANDE SOUHAITEE
!  IN   POUM   (K*) : /'+', /'-', /'REF'
!  IN   FAMI   (K8) : NOM DE LA FAMILLE DE POINTS DE GAUSS ('RIGI',...)
!  IN   KPG    (I)  : NUMERO DU POINT DE GAUSS
!  IN   KSP    (I)  : NUMERO DU SOUS-POINT DE GAUSS (1 SINON)
!  OUT  VALVRC (R)  : VALEUR DE LA VARIABLE DE COMMANDE
!  OUT  IRET   (I)  : CODE RETOUR : 0 -> OK
!                                   1 -> VARIABLE NON TROUVEE

    character(len=8) :: novr8
    integer :: nbcvrc, jvcnom
    common /caii14/nbcvrc,jvcnom

    character(len=16) :: option, nomte, nomtm
    common /cakk01/option,nomte,nomtm

    integer :: nute, jnbelr, jnoelr, iactif, jpnlfp, jnolfp, nblfpg
    common /caii11/nute,jnbelr,jnoelr,iactif,jpnlfp,jnolfp,nblfpg

    integer :: iel
    common /caii08/iel

    integer :: nfpgmx
    parameter (nfpgmx=10)
    integer :: nfpg, jfpgl, decala(nfpgmx), km, kp, kr, iredec, nb2vrc
    common /caii17/nfpg,jfpgl,decala,km,kp,kr,iredec
    real(kind=8) :: timed1, timef1, td1, tf1
    common /carr01/timed1,timef1,td1,tf1

    integer :: kcvrc, ibid, nbsp, kpgvrc
    integer :: iadzi, iazk24, kpgmat, vali(3), iprem
    integer :: k, itabm(7), itabp(7), itabr(7)
    character(len=24) :: valk(4)
    character(len=8) :: nomail
    real(kind=8) :: valvrm, valvrp, rundf
    save itabm,itabp,itabr,rundf
    data iprem /0/
! ---------------------------------------------------------------
    if (iprem .eq. 0) then
        rundf=r8nnem()
        iprem=1
    endif

!   0)  s'il n'y a pas de varc, on ne peut pas les trouver !
!   -----------------------------------------------------------
    if (nbcvrc .eq. 0) goto 998

    if (iactif .eq. 2) then
!       -- on vient de calc_point_mat
        ASSERT(fami.eq.'PMAT')
        call rcvarp(arret, novrc, poum, valvrc, iret)
        goto 999
    endif


!   1) calcul de kpgmat (fami,kpg) : numero du pg dans la
!      famille "mater" (associee a pvarcmr et pvarcpr) :
!   -----------------------------------------------------------
    k=indik8(zk8(jfpgl),fami,1,nfpg)
    if (k .eq. 0) then
        valk(1)=novrc
        valk(2)=fami
        valk(3)=option
        valk(4)=nomte
        call utmess('F', 'CALCULEL6_58', nk=4, valk=valk)
    endif
    kpgmat=decala(k)+kpg


!   2) calcul de kcvcrc :
!   ----------------------
    novr8=novrc
    kcvrc=indik8(zk8(jvcnom),novr8,1,nbcvrc)

!     -- si la cvrc n'est pas fournie, on rend "r8nnem"
    if (kcvrc .eq. 0) then
        iret=1
        if (arret .eq. ' ') then
            valvrc=rundf
            goto 999
        else
            call tecael(iadzi, iazk24)
            nomail=zk24(iazk24-1+3)(1:8)
            valk(1) = novr8
            valk(2) = nomail
            valk(3) = poum
            call utmess('F', 'CALCULEL4_69', nk=3, valk=valk)
        endif
    endif



!   3) calcul de itabx : on cherche a economiser l'appel a tecach
!   ------------------------------------------------------------
    if (poum .eq. '-' .or. (poum.eq.'+' .and. iredec.eq.1)) then
        if (iel .ne. km) then
            if (arret .ne. ' ') then
                call tecach('OOO', 'PVARCMR', 'L', ibid, nval=7,&
                            itab=itabm)
            else
                call tecach('NNN', 'PVARCMR', 'L', iret, nval=7,&
                            itab=itabm)
                if (iret .ne. 0) goto 998
            endif
            km=iel
        endif
    endif

    if (poum .eq. '+' .or. (poum.eq.'-' .and. iredec.eq.1)) then
        if (iel .ne. kp) then
            if (arret .ne. ' ') then
                call tecach('OOO', 'PVARCPR', 'L', ibid, nval=7,&
                            itab=itabp)
            else
                call tecach('NNN', 'PVARCPR', 'L', iret, nval=7,&
                            itab=itabp)
                if (iret .ne. 0) goto 998
            endif
            kp=iel
        endif
    endif

    if (poum .eq. 'REF') then
        if (iel .ne. kr) then
            if (arret .ne. ' ') then
                call tecach('OOO', 'PVARCRR', 'L', ibid, nval=7,&
                            itab=itabr)
            else
                call tecach('NNN', 'PVARCRR', 'L', iret, nval=7,&
                            itab=itabr)
                if (iret .ne. 0) goto 998
            endif
            kr=iel
        endif
    endif


!   4) calcul de valvrc :
!   ----------------------

    if (poum .eq. 'REF') then
        nb2vrc=itabr(6)
        if (nb2vrc .ne. nbcvrc) goto 998
        nbsp=itabr(7)
        kpgvrc=(kpgmat-1)*nbsp+ksp
        valvrc=zr(itabr(1) -1 + (kpgvrc-1)*nbcvrc + kcvrc)

    else if (poum.eq.'+' .and. iredec.eq.0) then
        nb2vrc=itabp(6)
        if (nb2vrc .ne. nbcvrc) goto 998
        nbsp=itabp(7)
        kpgvrc=(kpgmat-1)*nbsp+ksp
        valvrc=zr(itabp(1) -1 + (kpgvrc-1)*nbcvrc + kcvrc)

    else if (poum.eq.'-' .and. iredec.eq.0) then
        nb2vrc=itabm(6)
        if (nb2vrc .ne. nbcvrc) goto 998
        nbsp=itabm(7)
        kpgvrc=(kpgmat-1)*nbsp+ksp
        valvrc=zr(itabm(1) -1 + (kpgvrc-1)*nbcvrc + kcvrc)

    else if (iredec.eq.1) then
        nb2vrc=itabm(6)
        if (nb2vrc .ne. nbcvrc) goto 998
        nbsp=itabm(7)
        kpgvrc=(kpgmat-1)*nbsp+ksp
        valvrm=zr(itabm(1) -1 + (kpgvrc-1)*nbcvrc + kcvrc)

        nb2vrc=itabp(6)
        if (nb2vrc .ne. nbcvrc) goto 998
        nbsp=itabp(7)
        kpgvrc=(kpgmat-1)*nbsp+ksp
        valvrp=zr(itabp(1) -1 + (kpgvrc-1)*nbcvrc + kcvrc)

        if ((iisnan(valvrm).eq.0) .and. (iisnan(valvrp).eq.0)) then
            if (poum .eq. '-') then
                valvrc=valvrm+(td1-timed1)*(valvrp-valvrm)/(timef1-&
                timed1)
            else if (poum.eq.'+') then
                valvrc=valvrm+(tf1-timed1)*(valvrp-valvrm)/(timef1-&
                timed1)
            else
                ASSERT(.false.)
            endif
        else
            valvrc=rundf
        endif

    else
        ASSERT(.false.)
    endif

    iret=0
    if (iisnan(valvrc) .gt. 0) iret=1


!   5) traitement si iret=1
!   ---------------------------
    if (iret .eq. 1) then
        if (arret .eq. ' ') then
            valvrc=rundf
        else
            call tecael(iadzi, iazk24)
            nomail=zk24(iazk24-1+3)(1:8)
            valk(1) = novr8
            valk(2) = nomail
            call utmess('F', 'CALCULEL4_69', nk=2, valk=valk)
        endif
    endif
    goto 999




998  continue
    if (arret .eq. ' ') then
        valvrc=rundf
        iret=1
    else
        call tecael(iadzi, iazk24)
        vali(1)=nb2vrc
        vali(2)=nbcvrc
        valk(1)=zk24(iazk24-1+3)
        call utmess('F', 'CALCULEL6_67', sk=valk(1), ni=2, vali=vali)
    endif


999  continue


end subroutine
