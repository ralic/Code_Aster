subroutine mdchan(motfac, ioc, iliai, mdgene, typnum,&
                  repere, xjeu, nbnli, noecho, parcho)
    implicit  none
    include 'jeveux.h'
    include 'asterc/getvr8.h'
    include 'asterc/r8dgrd.h'
    include 'asterfort/angvx.h'
    include 'asterfort/angvxy.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/normev.h'
    include 'asterfort/orient.h'
    include 'asterfort/provec.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: ioc, iliai, nbnli
    real(kind=8) :: xjeu, parcho(nbnli, *)
    character(len=8) :: repere, noecho(nbnli, *)
    character(len=10) :: motfac
    character(len=16) :: typnum
    character(len=24) :: mdgene
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
!     ROUTINE APPELEE PAR MDCHOC
!     RECHERCHE DES ANGLES NAUTIQUES
!
! IN  : MOTFAC : 'CHOC', 'FLAMBAGE', 'ANTI_SISM'
! IN  : IOC    : NUMERO D'OCCURENCE
! IN  : ILIAI  : NUMERO DE LA LIAISON TRAITEE
! IN  : MDGENE : MODELE GENERALISE
! IN  : TYPNUM : TYPE DE LA NUMEROTATION
! IN  : REPERE : REPERE DU NOEUD DE CHOC = 'GLOBAL' OU 'LOCAL'
! IN  : XJEU   : JEU INITIAL
! IN  : NBNLI  : DIMENSION DES TABLEAUX (NBCHOC+NBSISM+NBFLAM)
! IN  : NOECHO : (ILIAI,9) = TYPE D'OBSTACLE
! OUT : PARCHO : PARAMETRE DE CHOC:
!                PARCHO(ILIAI,17)= SIN A
!                PARCHO(ILIAI,18)= COS A
!                PARCHO(ILIAI,19)= SIN B
!                PARCHO(ILIAI,20)= COS B
!                PARCHO(ILIAI,21)= SIN G
!                PARCHO(ILIAI,22)= COS G
!     ------------------------------------------------------------------
    integer :: n1, jnorm
    real(kind=8) :: txloc(3), tzloc(3), tyloc(3), ang(3), alpha, beta
    real(kind=8) :: normx(3), normy(3), angl, rnorm, rad
    integer :: iarg
!     ------------------------------------------------------------------
!
    rad = r8dgrd()
!
    if (motfac .eq. 'CHOC' .or. motfac .eq. 'FLAMBAGE') then
!          ------------------------------------------
        call getvr8(motfac, 'NORM_OBST', ioc, iarg, 3,&
                    txloc, n1)
        call getvr8(motfac, 'ANGL_VRIL', ioc, iarg, 1,&
                    angl, n1)
!
        if (n1 .ne. 0) then
            if (typnum .eq. 'NUME_DDL_SDASTER' .or. repere .eq. 'GLOBAL') then
                call angvx(txloc, alpha, beta)
                parcho(iliai,17) = sin(alpha)
                parcho(iliai,18) = cos(alpha)
                parcho(iliai,19) = sin(beta)
                parcho(iliai,20) = cos(beta)
            else
                call wkvect('&&MDCHAN.NORM', 'V V R', 3, jnorm)
                zr(jnorm) = txloc(1)
                zr(jnorm+1) = txloc(2)
                zr(jnorm+2) = txloc(3)
                call orient(mdgene, repere, jnorm, 1, normx,&
                            0)
                call angvx(normx, alpha, beta)
                parcho(iliai,17) = sin(alpha)
                parcho(iliai,18) = cos(alpha)
                parcho(iliai,19) = sin(beta)
                parcho(iliai,20) = cos(beta)
                call jedetr('&&MDCHAN.NORM')
            endif
            parcho(iliai,21) = sin(angl*rad)
            parcho(iliai,22) = cos(angl*rad)
!
        else if (noecho(iliai,9).eq.'BI_PLANY') then
            tyloc(1) = (parcho(iliai,11) - parcho(iliai,8))
            tyloc(2) = (parcho(iliai,12) - parcho(iliai,9))
            tyloc(3) = (parcho(iliai,13) - parcho(iliai,10))
            if (typnum .eq. 'NUME_DDL_SDASTER' .or. repere .eq. 'GLOBAL') then
                call angvxy(txloc, tyloc, ang)
                parcho(iliai,17) = sin(ang(1))
                parcho(iliai,18) = cos(ang(1))
                parcho(iliai,19) = sin(ang(2))
                parcho(iliai,20) = cos(ang(2))
                parcho(iliai,21) = sin(ang(3))
                parcho(iliai,22) = cos(ang(3))
            else
                call wkvect('&&MDCHAN.NORM', 'V V R', 3, jnorm)
                zr(jnorm) = txloc(1)
                zr(jnorm+1) = txloc(2)
                zr(jnorm+2) = txloc(3)
                call orient(mdgene, repere, jnorm, 1, normx,&
                            0)
                zr(jnorm) = tyloc(1)
                zr(jnorm+1) = tyloc(2)
                zr(jnorm+2) = tyloc(3)
                call orient(mdgene, repere, jnorm, 1, normy,&
                            0)
                call angvxy(normx, normy, ang)
                parcho(iliai,17) = sin(ang(1))
                parcho(iliai,18) = cos(ang(1))
                parcho(iliai,19) = sin(ang(2))
                parcho(iliai,20) = cos(ang(2))
                parcho(iliai,21) = sin(ang(3))
                parcho(iliai,22) = cos(ang(3))
                call jedetr('&&MDCHAN.NORM')
            endif
!
        else if (noecho(iliai,9).eq.'BI_PLANZ') then
            tzloc(1) = (parcho(iliai,11) - parcho(iliai,8))
            tzloc(2) = (parcho(iliai,12) - parcho(iliai,9))
            tzloc(3) = (parcho(iliai,13) - parcho(iliai,10))
            call provec(tzloc, txloc, tyloc)
            if (typnum .eq. 'NUME_DDL_SDASTER' .or. repere .eq. 'GLOBAL') then
                call angvxy(txloc, tyloc, ang)
                parcho(iliai,17) = sin(ang(1))
                parcho(iliai,18) = cos(ang(1))
                parcho(iliai,19) = sin(ang(2))
                parcho(iliai,20) = cos(ang(2))
                parcho(iliai,21) = sin(ang(3))
                parcho(iliai,22) = cos(ang(3))
            else
                call wkvect('&&MDCHAN.NORM', 'V V R', 3, jnorm)
                zr(jnorm) = txloc(1)
                zr(jnorm+1) = txloc(2)
                zr(jnorm+2) = txloc(3)
                call orient(mdgene, repere, jnorm, 1, normx,&
                            0)
                zr(jnorm) = tzloc(1)
                zr(jnorm+1) = tzloc(2)
                zr(jnorm+2) = tzloc(3)
                call orient(mdgene, repere, jnorm, 1, normy,&
                            0)
                call angvxy(normx, normy, ang)
                parcho(iliai,17) = sin(ang(1))
                parcho(iliai,18) = cos(ang(1))
                parcho(iliai,19) = sin(ang(2))
                parcho(iliai,20) = cos(ang(2))
                parcho(iliai,21) = sin(ang(3))
                parcho(iliai,22) = cos(ang(3))
                call jedetr('&&MDCHAN.NORM')
            endif
!
        else
            call u2mess('I', 'ALGORITH5_25')
            angl = 0.d0
            if (typnum .eq. 'NUME_DDL_SDASTER' .or. repere .eq. 'GLOBAL') then
                call angvx(txloc, alpha, beta)
                parcho(iliai,17) = sin(alpha)
                parcho(iliai,18) = cos(alpha)
                parcho(iliai,19) = sin(beta)
                parcho(iliai,20) = cos(beta)
            else
                call wkvect('&&MDCHAN.NORM', 'V V R', 3, jnorm)
                zr(jnorm) = txloc(1)
                zr(jnorm+1) = txloc(2)
                zr(jnorm+2) = txloc(3)
                call orient(mdgene, repere, jnorm, 1, normx,&
                            0)
                call angvx(normx, alpha, beta)
                parcho(iliai,17) = sin(alpha)
                parcho(iliai,18) = cos(alpha)
                parcho(iliai,19) = sin(beta)
                parcho(iliai,20) = cos(beta)
                call jedetr('&&MDCHAN.NORM')
            endif
            parcho(iliai,21) = sin(angl*rad)
            parcho(iliai,22) = cos(angl*rad)
        endif
!
    else if (motfac.eq.'ANTI_SISM') then
!              ---------------------
!
        parcho(iliai,30)= sqrt(xjeu)/2.d0
        parcho(iliai,31)= sqrt(xjeu)/2.d0
!
! --- VECTEUR NOEUD1 VERS NOEUD2
        tyloc(1) = (parcho(iliai,11) - parcho(iliai,8))
        tyloc(2) = (parcho(iliai,12) - parcho(iliai,9))
        tyloc(3) = (parcho(iliai,13) - parcho(iliai,10))
        call normev(tyloc, rnorm)
        if (rnorm .eq. 0.0d0) then
            call u2mess('F', 'ALGORITH5_26')
        endif
!
! --- DETERMINATION DES AXES LOCAUX
        if (abs(tyloc(3)) .le. abs(tyloc(1)) .and. abs(tyloc(3)) .le. abs(tyloc(2))) then
            tzloc(1) = -tyloc(2)
            tzloc(2) = tyloc(1)
            tzloc(3) = 0.d0
            elseif ( abs(tyloc(2)).le.abs(tyloc(1)) .and. abs(tyloc(2))&
        .le.abs(tyloc(3)) ) then
            tzloc(1) = -tyloc(3)
            tzloc(2) = 0.d0
            tzloc(3) = tyloc(1)
        else
            tzloc(1) = 0.d0
            tzloc(2) = -tyloc(3)
            tzloc(3) = tyloc(2)
        endif
        call provec(tyloc, tzloc, txloc)
        if (typnum .eq. 'NUME_DDL_SDASTER' .or. repere .eq. 'GLOBAL') then
            call angvxy(txloc, tyloc, ang)
            parcho(iliai,17) = sin(ang(1))
            parcho(iliai,18) = cos(ang(1))
            parcho(iliai,19) = sin(ang(2))
            parcho(iliai,20) = cos(ang(2))
            parcho(iliai,21) = sin(ang(3))
            parcho(iliai,22) = cos(ang(3))
        else
            call wkvect('&&MDCHAN.NORM', 'V V R', 3, jnorm)
            zr(jnorm) = txloc(1)
            zr(jnorm+1) = txloc(2)
            zr(jnorm+2) = txloc(3)
            call orient(mdgene, repere, jnorm, 1, normx,&
                        0)
            zr(jnorm) = tyloc(1)
            zr(jnorm+1) = tyloc(2)
            zr(jnorm+2) = tyloc(3)
            call orient(mdgene, repere, jnorm, 1, normy,&
                        0)
            call angvxy(normx, normy, ang)
            parcho(iliai,17) = sin(ang(1))
            parcho(iliai,18) = cos(ang(1))
            parcho(iliai,19) = sin(ang(2))
            parcho(iliai,20) = cos(ang(2))
            parcho(iliai,21) = sin(ang(3))
            parcho(iliai,22) = cos(ang(3))
            call jedetr('&&MDCHAN.NORM')
        endif
!
    endif
!
end subroutine
