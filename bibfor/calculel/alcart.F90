subroutine alcart(base, chinz, maz, nomgdz)
    implicit none
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
    include 'jeveux.h'
!
    include 'asterfort/jecrec.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nbec.h'
    include 'asterfort/scalai.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    character(len=19) :: chin
    character(len=*) :: chinz, maz, nomgdz
    character(len=8) :: ma, nomgd
    character(len=*) :: base
! ----------------------------------------------------------------------
!     ENTREES:
!       BASE : BASE DE CREATION POUR LA CARTE : G/V/L
!      CHINZ : NOM DE LA CARTE A CREER
!        MAZ : NOM DU MAILLAGE ASSOCIE
!     NOMGDZ : NOM DE LA GRANDEUR
!     NGDMX  : NOMBRE MAXIMUM DE COUPLES ( ENTITE,VALE) A STOCKER
!     NMAMX  : DIMEMSION MAXIMUM DE L'OBJET LIMA
!                  (I.E. SOMME DES NOMBRES DE MAILLES DES GROUPES
!                    TARDIFS DE MAILLES)
!
!     SORTIES:
!     ON ALLOUE CHIN.DESC , CHIN.VALE , CHIN.NOMA ,CHIN.NOLI
!     ET CHIN.LIMA
!
!      SUR LA VOLATILE CHIN.NOMCMP ET CHIN.VALV
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: nec, jdesc, gd, ncmpmx, ngdmx, nmamx
    character(len=8) :: scal
    character(len=1) :: bas2
    integer :: noma, jbid
    character(len=1) :: k1bid
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ibid, j1
!-----------------------------------------------------------------------
    call jemarq()
!     NGDMX ET NMAMX : SERVENT A DIMENSIONNER LES OBJETS
!      CES OBJETS SERONT AGRANDIS SI NECESSAIRE PAR NOCART
!     LE PARAMETRE NGDMX EST DELIBEREMENT MIS A 1 CAR IL Y A UN PROBLEME
!     AVEC LES CARTES CONSTANTES DANS CALCUL SI CELLE-CI ONT UNE
!     DIMENSION SUPERIEURE A 1
    ngdmx=1
    nmamx=1
!
    chin = chinz
    ma = maz
    nomgd = nomgdz
!
    bas2 = base
!
!     STOCKAGE DE MA:
!
    call wkvect(chin//'.NOMA', bas2//' V K8', 1, noma)
    zk8(noma-1+1) = ma
!
!
    call jenonu(jexnom('&CATA.GD.NOMGD', nomgd), gd)
    if (gd .eq. 0) call u2mesk('F', 'CALCULEL_3', 1, nomgd)
    nec = nbec(gd)
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx, k1bid)
    scal = scalai(gd)
!
!     ALLOCATION DE DESC:
!
    call wkvect(chin//'.DESC', bas2//' V I', 3+ngdmx*(2+nec), jdesc)
    call jeecra(chin//'.DESC', 'DOCU', ibid, 'CART')
    zi(jdesc-1+1) = gd
    zi(jdesc-1+2) = ngdmx
    zi(jdesc-1+3) = 0
!
!     ALLOCATION DE VALE:
!
    call wkvect(chin//'.VALE', bas2//' V '//scal(1:4), ngdmx*ncmpmx, j1)
!
!     ALLOCATION DE NOLI
!
    call wkvect(chin//'.NOLI', bas2//' V K24', ngdmx, j1)
!
!
!     ALLOCATION DE LIMA :
!     --------------------
    call jecrec(chin//'.LIMA', bas2//' V I', 'NU', 'CONTIG', 'VARIABLE',&
                ngdmx)
! -- ON SURDIMENSIONNE A CAUSE DE JEVEUX : PAS D'OBJET DE LONGUEUR 0
    call jeecra(chin//'.LIMA', 'LONT', nmamx+ngdmx, ' ')
! -- ON FAIT MONTER LA COLLECTION EN MEMOIRE
    call jeveuo(chin//'.LIMA', 'E', jbid)
!
!     ALLOCATION DES OBJETS DE TRAVAIL NECESSAIRES A NOCART:
    call wkvect(chin//'.NCMP', 'V V K8', ncmpmx, j1)
    call wkvect(chin//'.VALV', 'V V '//scal(1:4), ncmpmx, j1)
    call jedema()
end subroutine
