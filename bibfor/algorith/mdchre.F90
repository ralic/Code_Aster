subroutine mdchre(motfac, ioc, iliai, mdgene, typnum,&
                  repere, nbnli, parcho, lnoue2)
    implicit  none
    include 'jeveux.h'
!
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/orient.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: ioc, iliai, nbnli
    real(kind=8) :: parcho(nbnli, *)
    logical :: lnoue2
    character(len=8) :: repere
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
!     TRAITEMENT DU REPERE
!
! IN  : MOTFAC : 'CHOC', 'FLAMBAGE', 'ANTI_SISM'
! IN  : IOC    : NUMERO D'OCCURENCE
! IN  : ILIAI  : NUMERO DE LA LIAISON TRAITEE
! IN  : MDGENE : MODELE GENERALISE
! IN  : TYPNUM : TYPE DE LA NUMEROTATION
! OUT : REPERE : REPERE DU NOEUD DE CHOC = 'GLOBAL' OU 'LOCAL'
! IN  : NBNLI  : DIMENSION DES TABLEAUX (NBCHOC+NBSISM+NBFLAM)
! OUT : PARCHO : PARAMETRE DE CHOC:
!                PARCHO(ILIAI,14)= COOR ORIGINE OBSTACLE X REP GLOBAL
!                PARCHO(ILIAI,15)= COOR ORIGINE OBSTACLE Y REP GLOBAL
!                PARCHO(ILIAI,16)= COOR ORIGINE OBSTACLE Z REP GLOBAL
!                PARCHO(ILIAI,45)= NORMALE X
!                PARCHO(ILIAI,46)= NORMALE Y
!                PARCHO(ILIAI,47)= NORMALE Z
! IN  : LNOUE2 : CHOC DEFINIT ENTRE 2 NOEUDS
!     ------------------------------------------------------------------
    integer :: n1, iret, jcoord
    real(kind=8) :: tempo(3), dircho(3), coord(3), txno
    character(len=24) :: mdssno
    integer :: iarg
!     ------------------------------------------------------------------
!
    n1 = 0
    repere = '????????'
!
    if (motfac .eq. 'CHOC' .or. motfac .eq. 'FLAMBAGE') then
!          ------------------------------------------
        call getvtx(motfac, 'REPERE', ioc, iarg, 0,&
                    repere, n1)
        if (n1 .eq. 0) then
            repere = 'GLOBAL'
        else
            call getvtx(motfac, 'REPERE', ioc, iarg, 1,&
                        repere, n1)
        endif
        call getvr8(motfac, 'ORIG_OBST', ioc, iarg, 1,&
                    tempo, n1)
    endif
!
    n1 = -n1
    if (n1 .eq. 3) then
        call getvr8(motfac, 'ORIG_OBST', ioc, iarg, 3,&
                    tempo, n1)
        if (typnum .eq. 'NUME_DDL_SDASTER') then
            parcho(iliai,14) = tempo(1)
            parcho(iliai,15) = tempo(2)
            parcho(iliai,16) = tempo(3)
        else
            mdssno = mdgene(1:14)//'.MODG.SSNO'
            if (repere .eq. 'GLOBAL') then
                parcho(iliai,14) = tempo(1)
                parcho(iliai,15) = tempo(2)
                parcho(iliai,16) = tempo(3)
            else
                call jenonu(jexnom(mdssno, repere), iret)
                if (iret .eq. 0) then
                    call u2mess('F', 'ALGORITH5_39')
                endif
                call wkvect('&&MDCHOC.COORDO', 'V V R', 3, jcoord)
                zr(jcoord) = tempo(1)
                zr(jcoord+1) = tempo(2)
                zr(jcoord+2) = tempo(3)
                call orient(mdgene, repere, jcoord, 1, coord,&
                            1)
                parcho(iliai,14) = coord(1)
                parcho(iliai,15) = coord(2)
                parcho(iliai,16) = coord(3)
                call jedetr('&&MDCHOC.COORDO')
            endif
        endif
    else
        parcho(iliai,14) = (parcho(iliai,8)+parcho(iliai,11))/2.d0
        parcho(iliai,15) = (parcho(iliai,9)+parcho(iliai,12))/2.d0
        parcho(iliai,16) = (parcho(iliai,10)+parcho(iliai,13))/2.d0
    endif
!
    if (lnoue2) then
        dircho(1) = parcho(iliai,8)-parcho(iliai,11)
        dircho(2) = parcho(iliai,9)-parcho(iliai,12)
        dircho(3) = parcho(iliai,10)-parcho(iliai,13)
    else
        dircho(1) = parcho(iliai,8)-parcho(iliai,14)
        dircho(2) = parcho(iliai,9)-parcho(iliai,15)
        dircho(3) = parcho(iliai,10)-parcho(iliai,16)
    endif
!
    txno = sqrt(dircho(1)**2+dircho(2)**2+dircho(3)**2)
    if (txno .eq. 0.d0) txno = 1.d0
!
! --- DEBUG : UN TRAVAIL DOIT ETRE FAIT SI TXNO = 0.
!
    parcho(iliai,45) = dircho(1)/txno
    parcho(iliai,46) = dircho(2)/txno
    parcho(iliai,47) = dircho(3)/txno
!
end subroutine
