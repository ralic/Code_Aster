subroutine cglect(resu, modele, ndim, option, cas,&
                  typfis, nomfis, fonoeu, chfond, basfon,&
                  taillr, conf, lnoff, liss, ndeg, typdis)
    implicit none
!
#include "asterc/getfac.h"
#include "asterfort/cgleff.h"
#include "asterfort/cgtyfi.h"
#include "asterfort/cgveca.h"
#include "asterfort/cgvedo.h"
#include "asterfort/cgvefo.h"
#include "asterfort/cgveli.h"
#include "asterfort/cgvemf.h"
#include "asterfort/cgverc.h"
#include "asterfort/cgveth.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
    integer :: ndim, lnoff, ndeg
    character(len=8) :: resu, modele, typfis, nomfis, conf
    character(len=16) :: option, cas, typdis
    character(len=24) :: fonoeu, chfond, basfon, taillr, liss
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
!     SOUS-ROUTINE DE L'OPERATEUR CALC_G
!
!     BUT : LECTURE ET VERIFICATION DES OPERANDES
!
!  IN :
!  OUT :
!     RESU   : MOT-CLE RESULTAT
!     MODELE : MODELE ASSOCIE A RESU
!     NDIM   : DIMENSION DU MODELE
!     OPTION : MOT-CLE OPTION
!     CAS    : '2D', '3D_LOCAL' OU '3D_GLOBAL'
!     TYPFIS : TYPE D'OBJET POUR DECRIRE LE FOND DE FISSURE
!              'FONDFISS' OU 'FISSURE' OU 'THETA'
!     NOMFIS : NOM DE L'OBJET POUR DECRIRE LE FOND DE FISSURE
!     FONOEU : NOMS DES NOEUDS DU FOND DE FISSURE
!     CHFOND : COORDONNES DES POINTS/NOEUDS DU FOND DE FISSURE
!     BASFON : BASE LOCALE AU FOND DE FISSURE
!     TAILLR : TAILLES DE MAILLES CONNECTEES AUX NOEUDS
!     CONF  : CONFIGURATION DE LA FISSURE EN FEM
!
!     LNOFF  : NOMBRE DE NOEUDS (OU POINTS) DU FOND DE FISSURE
!     LISS   : TYPE DE LISSAGE (NOM UNIQUE CONTRACTE)
!     TYPDIS : TYPE DE DISCONTINUITE SI FISSURE XFEM 
!              'FISSURE' OU 'COHESIF'
! ======================================================================
!
    integer :: ier, nexci
!
    call jemarq()
!
!     RECUPERATION DE LA SD RESULTAT : RESU
    call getvid(' ', 'RESULTAT', scal=resu, nbret=ier)
!
!     RECUPERATION DE L'OPTION
    call getvtx(' ', 'OPTION', scal=option, nbret=ier)
!
!     DETERMINATION DU TYPFIS = 'FONDFISS' OU 'FISSURE' OU 'THETA'
!     ET RECUPERATION DE LA SD POUR DECRIRE LE FOND DE FISSURE : NOMFIS
!     TYPE DE DISCONTINUITE SI FISSURE XFEM: 'FISSURE' OU 'COHESIF'
    call cgtyfi(typfis, nomfis, typdis)
!
!     LECTURE DES CHARGES ET VERIFICATION DE LA COMPATIBILITE AVEC RESU
    if(typdis.ne.'COHESIF') then
        call getfac('EXCIT', nexci)
        call cgverc(resu, nexci)
    endif
!
!     RECUPERATION DU MODELE PUIS DE LA DIMENSION DU MODELE
    call dismoi('MODELE', resu, 'RESULTAT', repk=modele)
    call dismoi('DIM_GEOM', modele, 'MODELE', repi=ndim)
!
!     VERIFICATION DE LA COMPATIBILITE ENTRE NDIM ET OPTION
    call cgvedo(ndim, option)
!
!   CALCUL COHESIF OUVERT EN 3D UNIQUEMENT POUR L INSTANT
    if(ndim.eq.2.and.typdis.eq.'COHESIF') then
        call utmess('F', 'RUPTURE2_5')
    endif
!
!     DETERMINATION DU CAS : 2D, 3D LOCAL OU 3D GLOBAL
    call cgveca(ndim, option, cas)
!
!     VERIFICATION DE LA COMPATIBILITE ENTRE LA SD ASSOCIEE AU FOND
!     DE FISSURE ET LE MODELE
    call cgvemf(modele, typfis, nomfis, typdis)
!
!     VERIFICATION DE LA COMPATIBILITE ENTRE OPTION ET TYPE DE FISSURE
    call cgvefo(option, typfis, nomfis, typdis)
!
!     VERIFICATION DES DONNEES RELATIVES AU(X) CHAMP(S) THETA
    call cgveth(typfis, cas)
!
!     LECTURE DE LA DESCRIPTION DU FOND DE FISSURE
!     ET RECUPERATION DES OBJETS FONOEU, CHFOND, BASFON + LNOFF
    call cgleff(typfis, nomfis, fonoeu, chfond, basfon,&
                taillr, conf, lnoff)
!
!     VERIFICATION DES DONNEES RELATIVES AU LISSAGE
!     ET DETERMINATION DU LISSAGE (NOM UNIQUE CONTRACTE) : LISS ET NDEG
    call cgveli(typfis, typdis, cas, option, lnoff, liss,&
                ndeg)
!
    call jedema()
!
end subroutine
