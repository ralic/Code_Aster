subroutine reslo2(modele, ligrel, chvois, cvoisx, tabido)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/calcul.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exisd.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/megeom.h'
    include 'asterfort/resvoi.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    integer :: tabido(5)
    character(len=8) :: modele
    character(len=24) :: chvois
    character(len=*) :: ligrel
! ----------------------------------------------------------------------
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
!
!     BUT:
!         PREPARER LE CALCUL DE L'ESTIMATEUR D'ERREUR EN RESIDU.
!         RECHERCHER LES VOISINS DE CHAQUE ELEMENT ET RECUPERER
!         DES ADRESSES.
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   MODELE : NOM DU MODELE
! IN   LIGREL : NOM DU LIGREL
!
!      SORTIE :
!-------------
! OUT  CHVOIS : NOM DU CHAMP DES VOISINS
! OUT  CVOISX : POUR XFEM, NOM DU CHAMP DES VOISINS DES SOUS-ELEMENTS
! OUT  TABIDO : TABLEAU D'ENTIERS CONTENANT DES ADRESSES
!           1 : IATYMA : ADRESSE DU VECTEUR TYPE MAILLE (NUMERO <-> NOM)
!           2 : IAGD   : ADRESSE DU VECTEUR GRANDEUR (NUMERO <-> NOM)
!           3 : IACMP  : ADRESSE DU VECTEUR NOMBRE DE COMPOSANTES
!                  (NUMERO DE GRANDEUR <-> NOMBRE DE COMPOSANTES)
!           4 : ICONX1 : ADRESSE DE LA COLLECTION CONNECTIVITE
!           5 : ICONX2 : ADRESSE DU POINTEUR DE LONGUEUR DE LA
!                        CONNECTIVITE
! ......................................................................
!
!
!
!
    character(len=6) :: nompro
    parameter ( nompro = 'RESLO2' )
!
    integer :: nbpin
    parameter ( nbpin = 2 )
    integer :: nbpout
    parameter ( nbpout = 1 )
!
    integer :: iret, ibid, ier, nbtm, ity, nbgd, igd, ncmp
    integer :: iacmp, iagd, iatyma, iconx1, iconx2
    character(len=1) :: base
    character(len=8) :: lpain(nbpin), lpaout(nbpout), ma, typema, gd
    character(len=16) :: opt
    character(len=19) :: cnseto, loncha
    character(len=24) :: lchin(nbpin), lchout(nbpout), chgeom, kbid
    character(len=24) :: blan24, cvoisx
!
!
! ----------------------------------------------------------------------
!
!               123456789012345678901234
    blan24 = '                        '
!
    base = 'V'
    call megeom(modele, chgeom)
!
! ------- RECHERCHE DES VOISINS ----------------------------------------
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
!
    lpaout(1) = 'PVOISIN'
    chvois = '&&'//nompro//'.CH_VOISIN'
    lchout(1) = chvois
    opt = 'INIT_MAIL_VOIS'
    call calcul('C', opt, ligrel, 1, lchin,&
                lpain, 1, lchout, lpaout, base,&
                'OUI')
    call exisd('CHAMP_GD', lchout(1), iret)
    if (iret .eq. 0) then
        call u2mesk('F', 'CALCULEL2_88', 1, opt)
    endif
!
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                ma, ier)
    call resvoi(modele, ma, chvois)
!
! ------- SI LE MODELE EST XFEM ON CALCULE LA SD VOISIN ----------------
! ---------- POUR LES SOUS-ELEMENTS DES ELEMENTS XFEM ------------------
!
    cvoisx = blan24
    call jeexin(modele//'.FISS', iret)
!
    if (iret .ne. 0) then
        opt = 'CHVOIS_XFEM'
        cnseto = modele//'.TOPOSE.CNS'
        loncha = modele//'.TOPOSE.LON'
        cvoisx = '&&'//nompro//'.CH_CNINVX'
!
        lpain(1) = 'PCNSETO'
        lchin(1) = cnseto
        lpain(2) = 'PLONCHA'
        lchin(2) = loncha
        lpaout(1) = 'PCVOISX'
        lchout(1) = cvoisx
!
        call calcul('C', opt, ligrel, 2, lchin,&
                    lpain, 1, lchout, lpaout, base,&
                    'OUI')
!
        call exisd('CHAMP_GD', lchout(1), iret)
!
        if (iret .eq. 0) then
            call u2mesk('F', 'CALCULEL2_88', 1, opt)
        endif
!
    endif
!
! ----- CALCUL DE 5 ADRESSES : -----------------------------------------
!      IATYMA : ADRESSE DU VECTEUR TYPE MAILLE (NUMERO <-> NOM)
!      IAGD   : ADRESSE DU VECTEUR GRANDEUR (NUMERO <-> NOM)
!      IACMP : ADRESSE DU VECTEUR NOMBRE DE COMPOSANTES
!                     (NUMERO DE GRANDEUR <-> NOMBRE DE COMPOSANTES)
!      ICONX1 : ADRESSE DE LA COLLECTION CONNECTIVITE
!      ICONX2 : ADRESSE DU POINTEUR DE LONGUEUR DE LA CONNECTIVITE
!
    call jelira('&CATA.TM.NOMTM', 'NOMMAX', nbtm, kbid)
    call wkvect('&&'//nompro//'.TYPEMA', 'V V K8', nbtm, iatyma)
!
    do 1 ity = 1, nbtm
        call jenuno(jexnum('&CATA.TM.NOMTM', ity), typema)
        zk8(iatyma-1+ity) = typema
 1  end do
!
    call jelira('&CATA.GD.NOMGD', 'NOMMAX', nbgd, kbid)
    call wkvect('&&'//nompro//'.GD', 'V V K8', nbgd, iagd)
!
    do 2 igd = 1, nbgd
        call jenuno(jexnum('&CATA.GD.NOMGD', igd), gd)
        zk8(iagd-1+igd) = gd
 2  end do
!
    call wkvect('&&'//nompro//'.NBCMP', 'V V I', nbgd, iacmp)
!
    do 3 igd = 1, nbgd
        call jelira(jexnum('&CATA.GD.NOMCMP', igd), 'LONMAX', ncmp, kbid)
        zi(iacmp-1+igd) = ncmp
 3  end do
!
    call jeveuo(ma//'.CONNEX', 'L', iconx1)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', iconx2)
!
! ----- REMPLISSAGE DU TABLEAU -----------------------------------------
!
    tabido(1) = iatyma
    tabido(2) = iagd
    tabido(3) = iacmp
    tabido(4) = iconx1
    tabido(5) = iconx2
!
end subroutine
