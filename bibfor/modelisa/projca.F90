subroutine projca(tablca, lirela, nmabet, nbmabe, mailla,&
                  caelem, nbnobe, nunobe, icabl, nbnoca,&
                  xnoca, ynoca, znoca)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!  DESCRIPTION : PROJECTION DES NOEUDS D'UN CABLE SUR LE MAILLAGE BETON
!  -----------   ET DETERMINATION DES RELATIONS CINEMATIQUES ENTRE LES
!                DDLS DES NOEUDS DU CABLE ET LES DDLS DES NOEUDS VOISINS
!                DE LA STRUCTURE BETON
!                APPELANT : OP0180 , OPERATEUR DEFI_CABLE_BP
!
!                EN SORTIE ON AJOUTE DES LIGNES DANS LA TABLE RESULTAT
!                LES CASES RENSEIGNEES CORRESPONDENT AUX PARAMETRES
!                <MAILLE_BETON_VOISINE>, <NOEUD_BETON_VOISIN>,
!                <INDICE_PROJECTION> ET <EXCENTRICITE>
!                LA SD DE TYPE LISTE_DE_RELATIONS EST MISE A JOUR
!
!  IN     : TABLCA : CHARACTER*19
!                    NOM DE LA TABLE DECRIVANT LES CABLES
!  IN     : LIRELA : CHARACTER*19 , SCALAIRE
!                    NOM DE LA SD DE TYPE LISTE_DE_RELATIONS
!  IN     : NMABET : CHARACTER*24
!                    OBJET CONTENANT LES MAILLES BETON
!  IN     : NBMABE : INTEGER, SCALAIRE
!                    NOMBRE DE MAILLES BETON
!  IN     : MAILLA : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT MAILLAGE ASSOCIE A L'ETUDE
!  IN     : CAELEM : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT CARA_ELEM ASSOCIE A L'ETUDE
!  IN     : NBNOBE : INTEGER , SCALAIRE
!                    NOMBRE DE NOEUDS APPARTENANT A LA STRUCTURE BETON
!  IN     : NUNOBE : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR D'ENTIERS POUR STOCKAGE DES
!                    NUMEROS DES NOEUDS APPARTENANT A LA STRUCTURE BETON
!  IN     : ICABL  : INTEGER , SCALAIRE
!                    NUMERO DU CABLE
!  IN     : NBNOCA : INTEGER , VECTEUR DE DIMENSION NBCABL
!                    CONTIENT LES NOMBRES DE NOEUDS DE CHAQUE CABLE
!  IN     : XNOCA  : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR DE REELS POUR STOCKAGE DES
!                    ABSCISSES X DES NOEUDS APPARTENANT AUX CABLES
!  IN     : YNOCA  : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR DE REELS POUR STOCKAGE DES
!                    ORDONNEES Y DES NOEUDS APPARTENANT AUX CABLES
!  IN     : ZNOCA  : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR DE REELS POUR STOCKAGE DES
!                    COTES Z DES NOEUDS APPARTENANT AUX CABLES
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/projkm.h"
#include "asterfort/reci2d.h"
#include "asterfort/tbajli.h"
#include "asterfort/utmess.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=8) :: mailla, caelem
    character(len=19) :: lirela, nunobe, xnoca, ynoca, znoca, tablca
    integer :: nbnobe, icabl, nbmabe, nbnoca(*)
    character(len=24) :: nmabet
!
! VARIABLES LOCALES
! -----------------
    integer :: ideca, inobe, inoca, ipara, iproj, itria, jcoor, jnoca
    integer :: jnunob, jxca, jyca, jzca, nbcnx, nblign
    integer :: nbno, nbpara, nnomax, noe, noebe, numail
    real(kind=8) :: d2, d2min, dx, dy, dz, excent, normal(3), x3dca(3), xbar(3)
    real(kind=8) :: dmax_cable
    complex(kind=8) :: cbid
    character(len=8) :: nnoeca, voisin(2)
    character(len=24) :: coorno, nomama, nonoca, nonoma
    aster_logical :: encore
!
    character(len=24) :: param(4), parcr
    integer, pointer :: cnx_maille(:) => null()
    real(kind=8), pointer :: xyz_noemai(:) => null()
    character(len=24), pointer :: tblp(:) => null()
    integer, pointer :: tbnp(:) => null()
    data          param /'MAILLE_BETON_VOISINE    ',&
     &                     'NOEUD_BETON_VOISIN      ',&
     &                     'INDICE_PROJECTION       ',&
     &                     'EXCENTRICITE            '/
    data          parcr /'NOEUD_CABLE             '/
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
    cbid=(0.d0,0.d0)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   ACCES AUX DONNEES
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! 1.1 OBJETS DU MAILLAGE
! ---
    coorno = mailla//'.COORDO    .VALE'
    call jeveuo(coorno, 'L', jcoor)
    nomama = mailla//'.NOMMAI'
    nonoma = mailla//'.NOMNOE'
!
! 1.2 DONNEES RELATIVES AU CABLE
! ---
!.... NOMBRE DE NOEUDS
!
    nbno = nbnoca(icabl)
!
!.... NOMS DES NOEUDS
!
    call jeveuo(tablca//'.TBNP', 'L', vi=tbnp)
    nbpara = tbnp(1)
    nblign = tbnp(2)
    ideca = nblign - nbno
    call jeveuo(tablca//'.TBLP', 'L', vk24=tblp)
    do ipara = 1, nbpara
        if (tblp(1+4*(ipara-1)) .eq. parcr) then
            nonoca = tblp(1+4*(ipara-1)+2)
            call jeveuo(nonoca, 'L', jnoca)
            goto 11
        endif
    end do
 11 continue
!
!.... COORDONNEES DES NOEUDS
!
    call jeveuo(xnoca, 'L', jxca)
    call jeveuo(ynoca, 'L', jyca)
    call jeveuo(znoca, 'L', jzca)
!
! 1.3 NUMEROS DES NOEUDS DE LA STRUCTURE BETON
! ---
    call jeveuo(nunobe, 'L', jnunob)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   PROJECTION DES NOEUDS DU CABLE SUR LA STRUCTURE BETON
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! 2.1 CREATION D'OBJETS DE TRAVAIL
! ---
!.... LES MAILLES APPARTENANT A LA STRUCTURE BETON SONT DES MAILLES
!.... TRIA3, TRIA6, QUAD4, QUAD8 OU QUAD9 : LA VERIFICATION A ETE
!.... EFFECTUEE EN AMONT PAR LA ROUTINE TOMABE
!.... LE NOMBRE DE NOEUDS MAXIMAL SUR UNE MAILLE VAUT DONC 9
!
    nnomax = 9
    AS_ALLOCATE(vr=xyz_noemai, size=3*nnomax)
    AS_ALLOCATE(vi=cnx_maille, size=nnomax)
!
!   calcul de la dimension max du cable (approximativement)
!   principe : les noeuds des plus éloignés sont soit :
!       - les deux ancrages (cas des cables verticaux
!       - le premier noeud et le noeud milieu (cables horizontaux)
    dmax_cable = 0.d0
    inoca = 1 
    x3dca(1) = zr(jxca+ideca+inoca-1)
    x3dca(2) = zr(jyca+ideca+inoca-1)
    x3dca(3) = zr(jzca+ideca+inoca-1)
    inoca = nbno
    dx = x3dca(1) - zr(jxca+ideca+inoca-1)
    dy = x3dca(2) - zr(jyca+ideca+inoca-1)
    dz = x3dca(3) - zr(jzca+ideca+inoca-1)
    dmax_cable = sqrt(dx * dx + dy * dy + dz * dz)
    inoca = nbno/2
    dx = x3dca(1) - zr(jxca+ideca+inoca-1)
    dy = x3dca(2) - zr(jyca+ideca+inoca-1)
    dz = x3dca(3) - zr(jzca+ideca+inoca-1)
    d2 = sqrt(dx * dx + dy * dy + dz * dz)
    if (d2 .gt. dmax_cable) dmax_cable = d2
!
! 2.2 BOUCLE SUR LE NOMBRE DE NOEUDS DU CABLE
! ---
    do inoca = 1, nbno
        nnoeca = zk8(jnoca+ideca+inoca-1)
        x3dca(1) = zr(jxca+ideca+inoca-1)
        x3dca(2) = zr(jyca+ideca+inoca-1)
        x3dca(3) = zr(jzca+ideca+inoca-1)
!
        encore = .true.
!
! 2.2.1  DETERMINATION DU NOEUD DE LA STRUCTURE BETON LE PLUS PROCHE
! .....  DU NOEUD CABLE COURANT
!
        noebe = zi(jnunob)
        dx = x3dca(1) - zr(jcoor+3*(noebe-1) )
        dy = x3dca(2) - zr(jcoor+3*(noebe-1)+1)
        dz = x3dca(3) - zr(jcoor+3*(noebe-1)+2)
        d2min = dx * dx + dy * dy + dz * dz
        do inobe = 2, nbnobe
            noe = zi(jnunob+inobe-1)
            dx = x3dca(1) - zr(jcoor+3*(noe-1) )
            dy = x3dca(2) - zr(jcoor+3*(noe-1)+1)
            dz = x3dca(3) - zr(jcoor+3*(noe-1)+2)
            d2 = dx * dx + dy * dy + dz * dz
            if (d2 .lt. d2min) then
                d2min = d2
                noebe = noe
            endif
        enddo
!
! 2.2.2  TENTATIVE DE PROJECTION DU NOEUD CABLE
!
        call projkm(nmabet, nbmabe, nbnobe, mailla, caelem, dmax_cable, &
                    nnoeca, x3dca(1), noebe, numail, nbcnx,&
                    cnx_maille, xyz_noemai, normal(1), itria, xbar(1),&
                    iproj, excent)
        if (iproj .eq. -1) then
            call utmess('F', 'MODELISA5_82', si = icabl ,sk=nnoeca)
        endif
!
! 2.2.5  DETERMINATION DES RELATIONS CINEMATIQUES
! .....
        call reci2d(lirela, mailla, nnoeca, noebe, nbcnx,&
                    cnx_maille, normal(1), itria, xbar(1), iproj,&
                    excent)
!
! 2.2.6  MISE A JOUR DE LA SD TABLE
! .....
        call jenuno(jexnum(nomama, numail), voisin(1))
        call jenuno(jexnum(nonoma, noebe), voisin(2))
        call tbajli(tablca, 4, param, [iproj], [excent],&
                    [cbid], voisin(1), ideca+ inoca)
!
    end do
!
! --- MENAGE
    AS_DEALLOCATE(vr=xyz_noemai)
    AS_DEALLOCATE(vi=cnx_maille)
!
    call jedema()
!
! --- FIN DE PROJCA.
end subroutine
