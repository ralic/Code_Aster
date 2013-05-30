subroutine irmare(ifc, ndim, nno, coordo, nbma,&
                  connex, point, noma, typma, typel,&
                  lmod, titre, nbtitr, nbgrn, nbgrm,&
                  nomai, nonoe, formar)
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=80) :: titre(*)
    character(len=8) :: nomai(*), nonoe(*), noma
    character(len=16) :: formar
    real(kind=8) :: coordo(*)
    integer :: connex(*), typma(*), point(*), typel(*), ifc, nbtitr
    logical :: lmod
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT :   ECRITURE DU MAILLAGE AU FORMAT ASTER
!     ENTREE:
!       IFC    : NUMERO D'UNITE LOGIQUE DU FICHIER ASTER
!       NDIM   : DIMENSION DU PROBLEME (2  OU 3)
!       NNO    : NOMBRE DE NOEUDS DU MAILLAGE
!       COORDO : VECTEUR DES COORDONNEES DES NOEUDS
!       NBMA   : NOMBRE DE MAILLES DU MAILLAGE
!       CONNEX : CONNECTIVITES
!       POINT  : POINTEUR DANS LES CONNECTIVITES
!       NOMAT  : NOM DU MAILLAGE
!       TYPMA  : TYPES DES MAILLES
!       TYPEL  : TYPES DES ELEMENTS
!       LMOD   : LOGIQUE INDIQUANT SI IMPRESSION MODELE OU MAILLAGE
!                 .TRUE. : ON N'IMPRIME QUE LES MAILLES DU MODELE
!       TITRE  : TITRE ASSOCIE AU MAILLAGE
!       TOUT CE QUI SUIT CONCERNE LES GROUPES:
!          NBGRN: NOMBRE DE GROUPES DE NOEUDS
!          NBGRM: NOMBRE DE GROUPES DE MAILLES
!          NOMAI: NOMS DES MAILLES
!          NONOE: NOMS DES NOEUDS
!
!     ------------------------------------------------------------------
! ---------------------------------------------------------------------
!
    character(len=8) :: nomm, kbid
    character(len=10) :: format
    character(len=24) :: nomgr
    character(len=50) :: fmt
!
!     ECRITURE DU TITRE
!
!-----------------------------------------------------------------------
    integer :: i, iagrma, iagrno, ico, ifin, igm, ign
    integer :: ima, ino, ipo, ipoin, it, itype, itypp
    integer :: j, jm, jmai, jn, k, nbfois, nbgrm
    integer :: nbgrn, nbm, nbma, nbn, nbrest, ndim, nno
    integer :: nnoe
!-----------------------------------------------------------------------
    call jemarq()
    format = formar
    fmt='(1X,A8,1X,'//format//',1X,'//format//',1X,'//format//')'
    write (ifc,*)      'TITRE'
    do 10 it = 1, nbtitr
        write (ifc,'(A)') titre(it)
10  end do
    write (ifc,*)      'FINSF'
    write (ifc,*)      '%'
!
!
!     ECRITURE DES NOEUDS
!     --------------------
    if (ndim .eq. 3) then
        write (ifc,*) 'COOR_3D'
    else if (ndim.eq.2) then
        write (ifc,*) 'COOR_2D'
    else if (ndim.eq.1) then
        write (ifc,*) 'COOR_1D'
    else
        call u2mess('F', 'PREPOST2_77')
    endif
    do 1 ino = 1, nno
        write (ifc,fmt) nonoe(ino),(coordo(3*(ino-1)+j),j=1,ndim)
 1  end do
!
!
!     ECRITURE DES MAILLES
!     ---------------------
    itypp = 0
    ifin = 0
    do 21 ima = 1, nbma
        itype = typma(ima)
        ipoin=point(ima)
        nnoe=point(ima+1)-ipoin
!
!        -- SI LMOD =.TRUE. ON IMPRIME LE MODELE SINON LE MAILLAGE
        if (lmod) then
            if (typel(ima) .eq. 0) then
                goto 21
            endif
        endif
        if (itype .ne. itypp) then
            call jenuno(jexnum('&CATA.TM.NOMTM', itype), nomm)
            write(ifc,*) 'FINSF'
            write(ifc,*) '%'
            itypp = itype
            write(ifc,*) nomm
            ifin = 1
        endif
        nbfois = nnoe/7
        nbrest = nnoe - 7*nbfois
        if (nbfois .ge. 1) then
            write(ifc,1003) nomai(ima),(nonoe(connex(ipoin-1+i)),i=1,&
            7)
            ico = 8
            do 12 i = 2, nbfois
                write(ifc,1004) (nonoe(connex(ipoin-1+k)),k=ico,ico+6)
                ico=ico+7
12          continue
            if (nbrest .ne. 0) then
                write(ifc,1004) (nonoe(connex(ipoin-1+i)),i=ico,nnoe)
            endif
        else
            write(ifc,1003) nomai(ima), (nonoe(connex(ipoin-1+i)),i=1,&
            nnoe)
        endif
21  continue
    if (ifin .eq. 1) then
        write(ifc,*) 'FINSF'
        write(ifc,*) '%'
    endif
!
!
!     ECRITURE DES GROUPES DE NOEUDS
!     -------------------------------
    do 752 ign = 1, nbgrn
        call jenuno(jexnum(noma//'.GROUPENO', ign), nomgr)
        call jelira(jexnum(noma//'.GROUPENO', ign), 'LONUTI', nbn, kbid)
        write(ifc,*) 'GROUP_NO'
        write(ifc,*) nomgr
        if (nbn .gt. 0) then
            call jeveuo(jexnum(noma//'.GROUPENO', ign), 'L', iagrno)
            write(ifc,'(7(1X,A8))') (nonoe(zi(iagrno-1+jn)),jn=1,nbn)
        endif
        write(ifc,*) 'FINSF'
        write(ifc,*) '%'
752  end do
!
!
!     ECRITURE DES GROUPES DE MAILLES
!     --------------------------------
    do 754 igm = 1, nbgrm
        call jenuno(jexnum(noma//'.GROUPEMA', igm), nomgr)
        call jelira(jexnum(noma//'.GROUPEMA', igm), 'LONUTI', nbm, kbid)
        write(ifc,*) 'GROUP_MA'
        write(ifc,*) nomgr
        if (nbm .gt. 0) then
            call jeveuo(jexnum(noma//'.GROUPEMA', igm), 'L', iagrma)
            call wkvect('&&IRMARE.NOMAI', 'V V K8', max(nbm, 1), jmai)
            ipo = 0
            do 756 jm = 1, nbm
                if (lmod) then
                    if (typel(zi(iagrma-1+jm)) .eq. 0) goto 756
                endif
                zk8(jmai-1+ipo+1)= nomai(zi(iagrma-1+jm))
                ipo=ipo+1
756          continue
            if (ipo .ne. 0) then
                write(ifc,'(7(1X,A8))') (zk8(jmai-1+jm),jm=1,ipo)
            endif
            call jedetr('&&IRMARE.NOMAI')
        endif
        write(ifc,*) 'FINSF'
        write(ifc,*) '%'
754  end do
!
!
    write(ifc,*) 'FIN'
!
    1003 format (8(1x,a8))
    1004 format (9x,7(1x,a8))
    call jedema()
end subroutine
