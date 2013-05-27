subroutine delat(modgen, nbsst, nbmo)
    implicit none
!
!---------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!---------------------------------------------------------------------
! AUTEUR : G. ROUSSEAU
!
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/wkvect.h'
    integer :: ibid, nbid, isst
    character(len=8) :: k8bid
    character(len=8) :: modgen, macel
    complex(kind=8) :: cbid
! -----------------------------------------------------------------
!---------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ibamo, icompt, idelat, ij, imacl, jpara, nbmo
    integer :: nbmodg, nbsst, nbtype
    real(kind=8) :: bid, ebid
!-----------------------------------------------------------------------
    call jemarq()
!
! NB DE MODES TOTAL
!
    nbmo=0
    do 1 isst = 1, nbsst
        call jeveuo(jexnum(modgen//'      .MODG.SSME', isst), 'L', imacl)
        macel=zk8(imacl)
        call jeveuo(macel//'.MAEL_REFE', 'L', ibamo)
        call rsorac(zk24(ibamo), 'LONUTI', ibid, bid, k8bid,&
                    cbid, ebid, 'ABSOLU', nbmodg, 1,&
                    nbid)
        nbmo=nbmo+nbmodg
 1  continue
!
! TABLEAU INDIQUANT LES MODES PROPRES
!
    call wkvect('&&DELAT.INDIC', 'V V I', nbmo, idelat)
    icompt=0
    do 2 isst = 1, nbsst
        call jeveuo(jexnum(modgen//'      .MODG.SSME', isst), 'L', imacl)
        macel=zk8(imacl)
!
        call jeveuo(macel//'.MAEL_REFE', 'L', ibamo)
!
!       CALL JEVEUO(ZK24(IBAMO)(1:19)//'.TYPE','L',ITYPE)
        call jelira(zk24(ibamo)(1:19)//'.ORDR', 'LONUTI', nbtype, k8bid)
        do 3, ij=1,nbtype
        icompt=icompt+1
        call rsadpa(zk24(ibamo)(1:19), 'L', 1, 'TYPE_DEFO', ij,&
                    0, jpara, k8bid)
        if (zk16(jpara)(1:8) .ne. 'PROPRE  ') goto 3
        zi(idelat+icompt-1)=1
 3      continue
 2  continue
    call jedema()
end subroutine
