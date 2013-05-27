subroutine rcvarc(arret, novrc, poum, fami, kpg,&
                  ksp, valvrc, iret)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'jeveux.h'
!
    include 'asterc/iisnan.h'
    include 'asterc/indik8.h'
    include 'asterc/r8nnem.h'
    include 'asterfort/assert.h'
    include 'asterfort/rcvarp.h'
    include 'asterfort/tecach.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: novrc, poum, fami
    character(len=1) :: arret
    integer :: iret, kpg, ksp
    real(kind=8) :: valvrc
!-----------------------------------------------------------------------
! BUT: RECUPERER LA VALEUR D'UNE VARIABLE DE COMMANDE SUR UN SOUS-POINT
!      DE GAUSS (KPG,KSP) ET POUR UNE VALEUR D'INSTANT ('+','-','REF')
!
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
!
    character(len=8) :: novr8
    integer :: nbcvrc, jvcnom
    common /caii14/nbcvrc,jvcnom
!
    character(len=16) :: option, nomte, nomtm, pheno, modeli
    common /cakk01/option,nomte,nomtm,pheno,modeli
!
    integer :: nute, jnbelr, jnoelr, iactif, jpnlfp, jnolfp, nblfpg
    common /caii11/nute,jnbelr,jnoelr,iactif,jpnlfp,jnolfp,nblfpg
!
    integer :: iel
    common /caii08/iel
!
    integer :: nfpgmx
    parameter (nfpgmx=10)
    integer :: nfpg, jfpgl, decala(nfpgmx), km, kp, kr, iredec, nb2vrc
    common /caii17/nfpg,jfpgl,decala,km,kp,kr,iredec
    real(kind=8) :: timed1, timef1, td1, tf1
    common /carr01/timed1,timef1,td1,tf1
!
    integer :: kcvrc, ibid, nbsp, kpgvrc
    integer :: iadzi, iazk24, kpgmat, vali(3), iprem
    integer :: k, itabm(7), itabp(7), itabr(7)
    character(len=24) :: valk(4)
    character(len=8) :: nomail
    real(kind=8) :: valvrm, valvrp, valr(3), tdef, rundf
    save itabm,itabp,itabr,rundf
    data iprem /0/
! ---------------------------------------------------------------
    if (iprem .eq. 0) then
        rundf=r8nnem()
        iprem=1
    endif
!
!     -- S'IL N'Y A PAS DE VARC, ON NE PEUT PAS LES TROUVER !
    if (nbcvrc .eq. 0) goto 9998
!
    if (iactif .eq. 2) then
!        ON VIENT DE CALC_POINT_MAT
        call assert(fami.eq.'PMAT')
        call rcvarp(arret, novrc, poum, valvrc, iret)
        goto 9999
    endif
!
    tdef=rundf
!
!
!     1) CALCUL DE KPGMAT (FAMI,KPG) : NUMERO DU PG DANS LA
!        FAMILLE "MATER" (ASSOCIEE A PVARCMR ET PVARCPR) :
!     -----------------------------------------------------------
!     CALL ASSERT(FAMI.NE.'MATER')
!     CALL ASSERT(NFPG.NE.0)
    k=indik8(zk8(jfpgl),fami,1,nfpg)
    if (k .eq. 0) then
        if (arret .eq. ' ') then
            goto 9998
        else
            valk(1)=novrc
            valk(2)=fami
            valk(3)=option
            valk(4)=nomte
            call u2mesk('F', 'CALCULEL6_58', 4, valk)
        endif
    endif
    kpgmat=decala(k)+kpg
!
!
!     2) CALCUL DE KCVCRC :
!     ----------------------
    novr8=novrc
    kcvrc=indik8(zk8(jvcnom),novr8,1,nbcvrc)
!
!     -- SI LA CVRC N'EST PAS FOURNIE, ON REND "R8NNEM"
!
    if (kcvrc .eq. 0) then
        iret=1
        if (arret .eq. ' ') then
            valvrc=rundf
            goto 9999
        else
            call tecael(iadzi, iazk24)
            nomail=zk24(iazk24-1+3)(1:8)
            valk(1) = novr8
            valk(2) = nomail
            valk(3) = poum
            call u2mesk('F', 'CALCULEL4_69', 3, valk)
        endif
    endif
!
!
!
!     3) CALCUL DE ITABX : ON CHERCHE A ECONOMISER L'APPEL A TECACH
!     ------------------------------------------------------------
    if (poum .eq. '-' .or. (poum.eq.'+' .and. iredec.eq.1)) then
        if (iel .ne. km) then
            if (arret .ne. ' ') then
                call tecach('OOO', 'PVARCMR', 'L', 7, itabm,&
                            ibid)
            else
                call tecach('NNN', 'PVARCMR', 'L', 7, itabm,&
                            iret)
                if (iret .ne. 0) goto 9998
            endif
            km=iel
        endif
    endif
!
    if (poum .eq. '+' .or. (poum.eq.'-' .and. iredec.eq.1)) then
        if (iel .ne. kp) then
            if (arret .ne. ' ') then
                call tecach('OOO', 'PVARCPR', 'L', 7, itabp,&
                            ibid)
            else
                call tecach('NNN', 'PVARCPR', 'L', 7, itabp,&
                            iret)
                if (iret .ne. 0) goto 9998
            endif
            kp=iel
        endif
    endif
!
    if (poum .eq. 'REF') then
        if (iel .ne. kr) then
            if (arret .ne. ' ') then
                call tecach('OOO', 'PVARCRR', 'L', 7, itabr,&
                            ibid)
            else
                call tecach('NNN', 'PVARCRR', 'L', 7, itabr,&
                            iret)
                if (iret .ne. 0) goto 9998
            endif
            kr=iel
        endif
    endif
!
!
!     4) CALCUL DE VALVRC :
!     ----------------------
!
    if (poum .eq. 'REF') then
        nb2vrc=itabr(6)
        if (nb2vrc .ne. nbcvrc) goto 9998
!       NBPG=ITABR(3)
        nbsp=itabr(7)
!       CALL ASSERT((KPGMAT.GE.1).AND.(KPGMAT.LE.NBPG))
!       CALL ASSERT((KSP.GE.1).AND.(KSP.LE.NBSP))
        kpgvrc=(kpgmat-1)*nbsp+ksp
        valvrc=zr(itabr(1) -1 + (kpgvrc-1)*nbcvrc + kcvrc)
!
    else if (poum.eq.'+' .and. iredec.eq.0) then
        nb2vrc=itabp(6)
        if (nb2vrc .ne. nbcvrc) goto 9998
!       NBPG=ITABP(3)
        nbsp=itabp(7)
!       CALL ASSERT((KPGMAT.GE.1).AND.(KPGMAT.LE.NBPG))
!       CALL ASSERT((KSP.GE.1).AND.(KSP.LE.NBSP))
        kpgvrc=(kpgmat-1)*nbsp+ksp
        valvrc=zr(itabp(1) -1 + (kpgvrc-1)*nbcvrc + kcvrc)
!
    else if (poum.eq.'-' .and. iredec.eq.0) then
        nb2vrc=itabm(6)
        if (nb2vrc .ne. nbcvrc) goto 9998
!       NBPG=ITABM(3)
        nbsp=itabm(7)
!       CALL ASSERT((KPGMAT.GE.1).AND.(KPGMAT.LE.NBPG))
!       CALL ASSERT((KSP.GE.1).AND.(KSP.LE.NBSP))
        kpgvrc=(kpgmat-1)*nbsp+ksp
        valvrc=zr(itabm(1) -1 + (kpgvrc-1)*nbcvrc + kcvrc)
!
    else if (iredec.eq.1) then
        nb2vrc=itabm(6)
        if (nb2vrc .ne. nbcvrc) goto 9998
!       NBPG=ITABM(3)
        nbsp=itabm(7)
!       CALL ASSERT((KPGMAT.GE.1).AND.(KPGMAT.LE.NBPG))
!       CALL ASSERT((KSP.GE.1).AND.(KSP.LE.NBSP))
        kpgvrc=(kpgmat-1)*nbsp+ksp
        valvrm=zr(itabm(1) -1 + (kpgvrc-1)*nbcvrc + kcvrc)
!
        nb2vrc=itabp(6)
        if (nb2vrc .ne. nbcvrc) goto 9998
!       NBPG=ITABP(3)
        nbsp=itabp(7)
!       CALL ASSERT((KPGMAT.GE.1).AND.(KPGMAT.LE.NBPG))
!       CALL ASSERT((KSP.GE.1).AND.(KSP.LE.NBSP))
        kpgvrc=(kpgmat-1)*nbsp+ksp
        valvrp=zr(itabp(1) -1 + (kpgvrc-1)*nbcvrc + kcvrc)
!
        if ((iisnan(valvrm).eq.0) .and. (iisnan(valvrp).eq.0)) then
            if (poum .eq. '-') then
                valvrc=valvrm+(td1-timed1)*(valvrp-valvrm)/(timef1-&
                timed1)
            else if (poum.eq.'+') then
                valvrc=valvrm+(tf1-timed1)*(valvrp-valvrm)/(timef1-&
                timed1)
            else
                call assert(.false.)
            endif
        else
            valvrc=rundf
        endif
!
    else
        call assert(.false.)
    endif
!
    iret=0
    if (iisnan(valvrc) .gt. 0) iret=1
!
!
!     -- TRAITEMENT SI IRET=1
    if (iret .eq. 1) then
        if (novr8 .eq. 'TEMP') then
            valvrc=tdef
            iret=1
            goto 9999
        endif
        if (arret .eq. ' ') then
            valvrc=rundf
        else
            call tecael(iadzi, iazk24)
            nomail=zk24(iazk24-1+3)(1:8)
            valk(1) = novr8
            valk(2) = nomail
            call u2mesk('F', 'CALCULEL4_69', 2, valk)
        endif
    endif
    goto 9999
!
!
!
!
9998  continue
    if (arret .eq. ' ') then
        valvrc=rundf
        iret=1
        goto 9999
    endif
    call tecael(iadzi, iazk24)
    vali(1)=nb2vrc
    vali(2)=nbcvrc
    valk(1)=zk24(iazk24-1+3)
    call u2mesg('F', 'CALCULEL6_67', 1, valk, 2,&
                vali, 0, valr)
!
!
9999  continue
!
!
end subroutine
