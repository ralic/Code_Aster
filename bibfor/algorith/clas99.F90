subroutine clas99(nomres)
    implicit none
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
!***********************************************************************
!  P. RICHARD   DATE 09/07/91
!-----------------------------------------------------------------------
!  BUT : ROUTINE DE CREATION D'UNE BASE MODALE CLASSIQUE
!        BASE MODALE DE TYPE MIXTE CRAIG-BAMPTON, MAC-NEAL OU AUCUN
!
!
    include 'jeveux.h'
!
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterfort/camoat.h'
    include 'asterfort/camoch.h'
    include 'asterfort/camoco.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/moco99.h'
    include 'asterfort/rscrsd.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: vali
!
!
    character(len=24) :: valk
    character(len=8) :: nomres, intf, kbid
    character(len=19) :: numddl, raid, mass, raidlt
    complex(kind=8) :: cbid
    integer :: iarg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
! --- RECUPERATION DES CONCEPTS AMONT
!
!-----------------------------------------------------------------------
    integer :: i, ibid, ii, inor, lldesc, llref, lrang
    integer :: ltmome, ltnbmo, nbid, nbmod, nbmodo, nbmoma, nbmome
    integer :: nbmout, nbsdd
    real(kind=8) :: bid, ebid
!-----------------------------------------------------------------------
    call jemarq()
    call jeveuo(nomres//'           .REFD', 'L', llref)
    raid=zk24(llref)
    mass=zk24(llref+1)
    numddl=zk24(llref+3)
    intf=zk24(llref+4)
!
!----ON AJOUT .NUME POUR OBTENIR LE PROF_CHNO
    numddl(15:19)='.NUME'
!
! --- RECUPERATION DU NOMBRE DE MODE_MECA A PRENDRE EN COMPTE
!
    call getvid('CLASSIQUE', 'MODE_MECA', 1, iarg, 0,&
                kbid, nbmome)
    nbmome = -nbmome
!
! --- CREATION DES OBJETS TEMPORAIRES
!
    call wkvect('&&CLAS99.LIST.MODE_MECA', 'V V K8', nbmome, ltmome)
    call wkvect('&&CLAS99.LIST.NBMOD', 'V V I', nbmome, ltnbmo)
!
    call getvid('CLASSIQUE', 'MODE_MECA', 1, iarg, nbmome,&
                zk8(ltmome), ibid)
    call getvis('CLASSIQUE', 'NMAX_MODE', 1, iarg, 1,&
                nbmout, nbid)
!
! --- DETERMINATION DU NOMBRE TOTAL DE MODES PROPRES DE LA BASE
!
    nbmod = 0
    nbmoma = 0
!
    do 5 i = 1, nbmome
        call rsorac(zk8(ltmome-1+i), 'LONUTI', ibid, bid, kbid,&
                    cbid, ebid, 'ABSOLU', nbmodo, 1,&
                    nbid)
!
        if (nbmodo .lt. nbmout) then
            call u2mess('I', 'ALGORITH15_92')
            valk = zk8(ltmome-1+i)
            call u2mesg('I', 'ALGORITH15_93', 1, valk, 0,&
                        0, 0, 0.d0)
            vali = nbmodo
            call u2mesg('I', 'ALGORITH15_94', 0, ' ', 1,&
                        vali, 0, 0.d0)
        else
            nbmodo=nbmout
        endif
!
        zi(ltnbmo+i-1) = nbmodo
        nbmoma = max(nbmoma,nbmodo)
        nbmod = nbmod+nbmodo
 5  end do
!
    call wkvect('&&CLAS99.NUME.RANG', 'V V I', nbmoma, lrang)
    do 10 ii = 1, nbmoma
        zi(lrang+ii-1)=ii
10  end do
!
!
! --- DETERMINATION NOMBRE TOTAL DE MODES ET DEFORMEES
!
    call jeveuo(intf//'.IDC_DESC', 'L', lldesc)
    nbsdd=nbmod+zi(lldesc+4)
!      NBSDD1=ZI(LLDESC+4)
!
!
!
! --- NOMBRE DE DEFORMEES STATIQUES A CALCULER
!
!
! --- ALLOCATION DE LA STRUCTURE DE DONNEES MODE_MECA
!
    call rscrsd('G', nomres, 'MODE_MECA', nbsdd)
    raidlt=' '
!
! --- COPIE DES MODES DYNAMIQUES
!
    inor=1
    do 6 i = 1, nbmome
        call moco99(nomres, zk8(ltmome+i-1), zi(ltnbmo+i-1), zi(lrang), inor,&
                    .true.)
 6  end do
    if (nbmoma .gt. 0) call jedetr('&&CLAS99.NUME.ORD')
    if (nbmome .gt. 0) call jedetr('&&CLAS99.LIST.MODE_MECA')
    if (nbmome .gt. 0) call jedetr('&&CLAS99.LIST.NBMOD')
!      CALL UTIMSD(6,2,.TRUE.,.TRUE.,NOMRES(1:8),1,'G')
!
! --- CALCUL DES MODES D'ATTACHE
    call camoat(nomres, numddl, intf, raid, raidlt,&
                inor)
!
! --- CALCUL DES MODES CONTRAINTS
    call camoco(nomres, numddl, intf, raid, raidlt,&
                inor)
!
! --- CALCUL DES MODES CONTRAINTS HARMONIQUES
    call camoch(nomres, numddl, intf, raid, mass,&
                raidlt, inor)
!
! --- DESTRUCTION MATRICE FACTORISEE
!
    if (raidlt(1:1) .ne. ' ') then
        call detrsd('MATR_ASSE', raidlt)
        raidlt=' '
    endif
!
    call jedema()
end subroutine
