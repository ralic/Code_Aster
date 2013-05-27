subroutine xprls(noma, cnsln, cnslt, grln, grlt,&
                 cnsvn, cnsvt, cnsbl, deltat, nodtor,&
                 eletor, liggrd, delta)
    implicit none
    include 'jeveux.h'
    include 'asterc/r8miem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/calcul.h'
    include 'asterfort/celces.h'
    include 'asterfort/cescns.h'
    include 'asterfort/cnscno.h'
    include 'asterfort/cnscre.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    real(kind=8) :: deltat
    character(len=8) :: noma
    character(len=19) :: cnsln, cnslt, grln, grlt, cnsvn, cnsvt, cnsbl, nodtor
    character(len=19) :: eletor, liggrd, delta
!
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
! person_in_charge: patrick.massin at edf.fr
!     ------------------------------------------------------------------
!
!       XPRLS   : X-FEM PROPAGATION DES LEVEL SETS
!       -----     -     --              -     -
!    PROPAGATION DES LEVEL SETS AU PAS DE TEMP SUIVANT
!
!    ENTREE
!        MODEL   : NOM DU CONCEPT MODELE
!        NOMA    : NOM DU CONCEPT MAILLAGE
!        CNSLT   : CHAM_NO_S LEVEL SET TANGENTIELLE
!        CNSLN   : CHAM_NO_S LEVEL SET NORMALE
!        GRLT    : CHAM_NO_S GRADIENT DE LEVEL SET TANGENTIELLE
!        GRLN    : CHAM_NO_S GRADIENT DE LEVEL SET NORMALE
!        CNSVN   : CHAM_NO_S DES COMPOSANTES NORMALES DE LA VITESSE DE
!                  PROPAGATION
!        CNSVT   : CHAM_NO_S DES COMPOSANTES TANGENTES DE LA VITESSE DE
!                  PROPAGATION
!        CNSBL   : CHAM_NO_S DES VECTEURS NORMALE ET TANGENTIELLE DE LA
!                  BASE LOCALE
!                  IN CHAQUE NODE DU MAILLAGE
!        DELTAT  : TEMPS TOTAL D'INTEGRATION
!        NODTOR  : LISTE DES NOEUDS DEFINISSANT LE DOMAINE DE CALCUL
!        ELETOR  : LISTE DES ELEMENTS DEFINISSANT LE DOMAINE DE CALCUL
!        LIGGRD  : LIGREL DU DOMAINE DE CALCUL (VOIR XPRTOR.F)
!        DELTA   : VECTEUR CONTENANT LES CORRECTIONS A APPORTER AU
!                  LEVELS SETS
!
!    SORTIE
!        CNSLT   : CHAM_NO_S LEVEL SET TANGENTIELLE
!        CNSLN   : CHAM_NO_S LEVEL SET NORMALE
!        GRLT    : CHAM_NO_S GRADIENT DE LEVEL SET TANGENTIELLE
!        GRLN    : CHAM_NO_S GRADIENT DE LEVEL SET NORMALE
!
!     ------------------------------------------------------------------
!
!
    integer :: i, ifm, niv, nbno, iret, jltno, jlnno, jgrtno, jgrnno, ndim, j
    integer :: jelcal, jnodto, node, nbnoma, ier
    integer :: ibid, neleto, jdelta
    character(len=8) :: k8b, lpain(2), lpaout(1)
    character(len=19) :: chgrlt, chgrln, chams, cnolt, cnoln
    character(len=24) :: lchin(2), lchout(1)
    real(kind=8) :: normgn, normgt
!
    real(kind=8) :: vnscgn, vtscgt
    character(len=8) :: typcmp(3), kbid
    character(len=19) :: cnsvvt, cnsvvn
    integer :: jvtv, jvtl, jvnv, jvnl, jcnsvn, jcnsvt
!
    integer :: jbl
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
!     RETRIEVE THE LOCAL REFERENCE SYSTEM FOR EACH NODE IN THE MESH
    call jeveuo(cnsbl//'.CNSV', 'E', jbl)
!
!     RECUPERATION DE CARACTERISTIQUES DU MAILLAGE
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnoma,&
                k8b, iret)
!
!     RETRIEVE THE DIMENSION OF THE PROBLEM (2D AND 3D ARE SUPPORTED)
    call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndim,&
                kbid, iret)
!
!     RETRIEVE THE NUMBER OF THE NODES THAT MUST TO BE USED IN THE
!     CALCULUS (SAME ORDER THAN THE ONE USED IN THE CONNECTION TABLE)
    call jeveuo(nodtor, 'L', jnodto)
!
!     RETRIEVE THE TOTAL NUMBER OF THE NODES THAT MUST BE ELABORATED
    call jelira(nodtor, 'LONMAX', nbno, k8b)
!
!     RETRIEVE THE LIST OF THE ELEMENTS SUPPORTING THE NODES IN THE TORE
    call jeveuo(eletor, 'L', jelcal)
!
!     RETRIEVE THE NUMBER OF ELEMENTS DEFINING THE TORE
    call jelira(eletor, 'LONMAX', neleto, k8b)
!
!     RETRIEVE THE correction to give to the level set at the node
!     projeted on the virtual front
    call jeveuo(delta, 'L', jdelta)
!
! ***************************************************************
! CALCULATE THE NORMAL AND TANGENTIAL PROPAGATION SPEED VECTOR FOR
! EACH NODE IN THE MESH (WITH RESPECT TO THE CRACK PLANE).
! ***************************************************************
!
!     CREATION OF THE NORMAL AND TANGENTIAL PROPAGATION SPEED VECTORS
!     DATA STRUCTURES (CHAMP_NO_S)
    cnsvvt = '&&XPRLS.CNSVT'
    cnsvvn = '&&XPRLS.CNSVN'
    typcmp(1)='X1'
    typcmp(2)='X2'
    typcmp(3)='X3'
    call cnscre(noma, 'NEUT_R', ndim, typcmp, 'V',&
                cnsvvt)
    call cnscre(noma, 'NEUT_R', ndim, typcmp, 'V',&
                cnsvvn)
!
    call jeveuo(cnsvvt//'.CNSV', 'E', jvtv)
    call jeveuo(cnsvvt//'.CNSL', 'E', jvtl)
    call jeveuo(cnsvvn//'.CNSV', 'E', jvnv)
    call jeveuo(cnsvvn//'.CNSL', 'E', jvnl)
!
!     RETRIEVE THE GRADIENT OF THE TWO LEVEL SETS
    call jeveuo(grlt//'.CNSV', 'E', jgrtno)
    call jeveuo(grln//'.CNSV', 'E', jgrnno)
!
!     RETRIEVE THE NORMAL AND TANGENTIAL PROPAGATION SPEEDS (SCALAR
!     VALUE). THESE VALUES WILL BE USED BELOW TO CALCULATE THE
!     PROPAGATION SPEED VECTORS FOR EACH NODE
    call jeveuo(cnsvn//'.CNSV', 'L', jcnsvn)
    call jeveuo(cnsvt//'.CNSV', 'L', jcnsvt)
!
!     ELABORATE EACH NODE IN THE TORE
    do 400 i = 1, nbno
!
!         RETREIVE THE NODE NUMBER
        node = zi(jnodto-1+i)
!
!           EVALUATE THE SPEED VECTORS
        call jeveuo(cnslt//'.CNSV', 'E', jltno)
!
!           CHECK IF THE NODE IS ON THE EXISTING CRACK SURFACE
        if (zr(jltno-1+node) .lt. r8miem()) then
!
!              CALCULATE THE NORM OF THE GRADIENTS IN ORDER TO EVALUATE
!              THE NORMAL AND TANGENTIAL UNIT VECTORS
            if (ndim .eq. 2) then
                normgt = (zr( jgrtno-1+2*(node-1)+1)**2.d0 + zr( jgrtno-1+2*(node-1)+2 )**2.d0&
                         )**.5d0
                normgn = (zr( jgrnno-1+2*(node-1)+1)**2.d0 + zr( jgrnno-1+2*(node-1)+2 )**2.d0&
                         )**.5d0
            else
                normgt = (&
                         zr(&
                         jgrtno-1+3*(node-1)+1)**2.d0 + zr( jgrtno-1+3*(node-1)+2)**2.d0 + zr(jgr&
                         &tno-1+3*(node-1)+ 3&
                         )**2.d0&
                         )**.5d0
                normgn = (&
                         zr(&
                         jgrnno-1+3*(node-1)+1)**2.d0 + zr( jgrnno-1+3*(node-1)+2)**2.d0 + zr(jgr&
                         &nno-1+3*(node-1)+ 3&
                         )**2.d0&
                         )**.5d0
            endif
!
!              IF THE TANGENTIAL LEVELSET IS NEGATIVE, THE NODE BELONGS
!              TO THE EXISTING CRACK SURFACE. THEREFORE THE GRADIENT OF
!              THE LEVEL SETS IS A GOOD CANDIDATE FOR THE LOCAL
!              REFERENCE SYSTEM.
            do 405 j = 1, ndim
                if (normgn .gt. r8prem()) then
                    zr(jvnv-1+ndim*(node-1)+j) = zr(jcnsvn-1+node)* zr(jgrnno-1+ndim*(node-1)+j)/&
                                                 &normgn
                else
                    zr(jvnv-1+ndim*(node-1)+j) = 0.d0
                endif
!
                if (normgt .gt. r8prem()) then
                    zr(jvtv-1+ndim*(node-1)+j) = zr(jcnsvt-1+node)* zr(jgrtno-1+ndim*(node-1)+j)/&
                                                 &normgt
                else
                    zr(jvtv-1+ndim*(node-1)+j) = 0.d0
                endif
405          continue
!
        else
!
!              IF THE TANGENTIAL LEVELSET IS POSITIVE, THE LOCAL
!              REFERENCE SYSTEM CALCULATED PREVIOUSLY FROM THE
!              INFORMATIONS ON THE CRACK FRONT CAN BE USED
            do 406 j = 1, ndim
                zr(jvnv-1+ndim*(node-1)+j) = zr(jcnsvn-1+node)* zr(jbl-1+2*ndim*(node-1)+j)
                zr(jvtv-1+ndim*(node-1)+j) = zr(jcnsvt-1+node)* zr(jbl-1+2*ndim*(node-1)+ndim+j)
406          continue
!
        endif
!
400  end do
!
! ***************************************************************
! UPDATE THE LEVEL SETS
! ***************************************************************
!
!     CREATION DES OBJETS VOLATILES
    chgrlt = '&&XPRLS.CHGRLT'
    chgrln = '&&XPRLS.CHGRLN'
    chams = '&&XPRLS.CHAMS'
    cnolt = '&&XPRLS.CNOLT'
    cnoln = '&&XPRLS.CNOLN'
!
!     RECUPERATION DE L'ADRESSE DES VALEURS DE LT, LN ET LEURS GRADIENTS
    call jeveuo(cnslt//'.CNSV', 'E', jltno)
    call jeveuo(cnsln//'.CNSV', 'E', jlnno)
    call jeveuo(grlt//'.CNSV', 'E', jgrtno)
    call jeveuo(grln//'.CNSV', 'E', jgrnno)
!
!     UPDATE THE LEVEL SETS FOR EACH NODE IN THE TORE
    do 100 i = 1, nbno
!
!         RETREIVE THE NODE NUMBER
        node = zi(jnodto-1+i)
!
        vnscgn = 0.d0
        vtscgt = 0.d0
!
        do 105 j = 1, ndim
!
!            SCALAR PRODUCT BETWEEN THE NORMAL PROPAGATION SPEED
!            VECTOR AND THE NORMAL GRADIENT
            vnscgn = vnscgn + zr( jvnv-1+ndim*(node-1)+j)*zr(jgrnno-1+ ndim*(node-1)+j)
!
!            SCALAR PRODUCT BETWEEN THE TANGENTIAL PROPAGATION SPEED
!            VECTOR AND  THE TANGENTIAL GRADIENT
            vtscgt = vtscgt + zr( jvtv-1+ndim*(node-1)+j)*zr(jgrtno-1+ ndim*(node-1)+j)
!
!
!
105      continue
!
!         UPDATE THE LEVEL SETS
        if (zr(jltno-1+node) .gt. r8prem()) then
            zr(jlnno-1+node)=zr(jlnno-1+node)-deltat*vnscgn+ zr(&
            jdelta+2*(node-1))
        else
            zr(jlnno-1+node)=zr(jlnno-1+node)-deltat*vnscgn
        endif
        zr(jltno-1+node)=zr(jltno-1+node)-deltat*vtscgt +zr(jdelta+2*(&
        node-1)+1)
!
!
100  end do
!
!-----------------------------------------------------------------------
!     CALCUL DES GRADIENTS DES LEVEL SETS RESULTANTES
!-----------------------------------------------------------------------
!
!  GRADIENT DE LT
    call cnscno(cnslt, ' ', 'NON', 'V', cnolt,&
                'F', ibid)
!
    lpain(1) = 'PGEOMER'
    lchin(1) = noma//'.COORDO'
    lpain(2) = 'PNEUTER'
    lchin(2) = cnolt
    lpaout(1)= 'PGNEUTR'
    lchout(1)= chgrlt
!
    call calcul('S', 'GRAD_NEUT_R', liggrd, 2, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
    call celces(chgrlt, 'V', chams)
    call cescns(chams, ' ', 'V', grlt, ' ',&
                ier)
!
!  GRADIENT DE LN
    call cnscno(cnsln, ' ', 'NON', 'V', cnoln,&
                'F', ibid)
!
    lpain(1) = 'PGEOMER'
    lchin(1) = noma//'.COORDO'
    lpain(2) = 'PNEUTER'
    lchin(2) = cnoln
    lpaout(1)= 'PGNEUTR'
    lchout(1)= chgrln
!
    call calcul('S', 'GRAD_NEUT_R', liggrd, 2, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
    call celces(chgrln, 'V', chams)
    call cescns(chams, ' ', 'V', grln, ' ',&
                ier)
!
!  DESTRUCTION DES OBJETS VOLATILES
    call jedetr(chgrlt)
    call jedetr(chgrln)
    call jedetr(chams)
    call jedetr(cnolt)
    call jedetr(cnoln)
    call jedetr(cnsvvt)
    call jedetr(cnsvvn)
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
