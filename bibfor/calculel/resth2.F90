subroutine resth2(modele, ligrel, lchar, nchar, ma,&
                  cartef, nomgdf, carteh, nomgdh, cartet,&
                  nomgdt, cartes, nomgds, chgeom, chsour,&
                  psourc)
!-----------------------------------------------------------------------
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
! person_in_charge: olivier.boiteau at edf.fr
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  PREPARATION DU CALCUL DE L'ESTIMATEUR
!                          D'ERREUR EN RESIDU SUR LE PROBLEME THERMIQUE.
!
! IN MODELE  : NOM DU MODELE
! IN LIGREL  : NOM DU LIGREL
! IN LCHAR   : LISTE DES CHARGES
! IN NCHAR   : NOMBRE DE CHARGES
! OUT MA     : NOM DU MAILLAGE
! OUT CARTEF/NOMGDF: INFO SUR LE FLUX RETENU
! OUT CARTEH/NOMGDH: INFO SUR L'ECHANGE RETENU
! OUT CARTET/NOMGDT: INFO SUR LA TEMP_EXT RETENUE
! OUT CARTES/NOMGDS: INFO SUR LA SOURCE RETENUE
! OUT CHGEOM : CHAMP GEOMETRIE
! OUT CHSOUR : CHAMP SOURCE
! OUT PSOURC : NOM DU PARAMETRE ASSOCIE A SOURCE
!
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES:
!       MESSAGE:U2MESS,U2MESG.
!       JEVEUX:JEMARQ,JEDEMA,JEDETR,MEGEOM,DISMOI,EXISD,ETENCA.
!       ELEMENTS FINIS:CALCUL,RESVOI.
!
!     FONCTIONS INTRINSEQUES:
!       AUCUNE.
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       22/08/02 (OB): CREATION DU A LA SEPARATION DE RESTHE, EN UNE
!          PARTIE PRELIMINAIRE (RESTH2) HORS DE LA BOUCLE EN TEMPS ET
!          UNE PARTIE (RESTHE) CALCUL DEPENDANT DU TEMPS.
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/dismoi.h"
#include "asterfort/etenca.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/megeom.h"
#include "asterfort/resvoi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: nchar
    character(len=8) :: modele, lchar(1), ma, psourc
    character(len=19) :: cartef, carteh, cartet, cartes, nomgdf, nomgdh, nomgdt
    character(len=19) :: nomgds
    character(len=24) :: ligrel, chgeom, chsour
!
! DECLARATION VARIABLES LOCALES
    integer :: i, ibid, ier, iretf, ireth, irett, irets, nbin, nbout, iretep
    character(len=1) :: base
    character(len=8) :: lpain(1), lpaout(1)
    character(len=16) :: opt
    character(len=19) :: cartf, carth, cartt, carts, cartep
    character(len=24) :: lchin(1), lchout(1)
!
! DEBUT DE LA SUBROUTINE
    call jemarq()
!
    ASSERT(ligrel(1:8).eq.modele)
!
! RECHERCHE DU NOM DU CHAMP GEOMETRIE DANS LA SD MODELE OU CHARGE -----
! SURCOUCHE DE LA REQUETE D'EXISTENCE JEEXIN/JEVEUO DU DESCRIPTEUR DE
! MODELE//'MODELE.NOMA.COORDO': RESULTAT DANS CHGEOM. ON S'ASSURE DE
! LA COHERENCE AVEC CHARGE//'CHTH.MODEL.NOMO'.
    base = 'V'
    call megeom(modele, chgeom)
!
! RECHERCHE DES ELTS FINIS CONTIGUS ET REMPLISSAGE DU CHAMELEM DE TYPE
! VOISIN VIA L'OPTION DE CALCUL 'INIT_MAIL_VOIS'.
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpaout(1) = 'PVOISIN'
    lchout(1) = '&&RESTHER.VOISIN'
    opt = 'INIT_MAIL_VOIS'
    nbin = 1
    nbout = 1
    call calcul('S', opt, ligrel, nbin, lchin,&
                lpain, nbout, lchout, lpaout, base,&
                'OUI')
!
! SURCOUCHE DE DISMMO RENVOYANT LE NOM DU MAILLAGE (MA) VIA UN JEVEUO
! SUR MODELE//'.MODELE.NOMA'
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                ma, ier)
! REMPLISSAGE DU CHAM_ELEM '&&RESTHER.VOISIN' PAR LES NUMEROS ET LES
! TYPES DE MAILLES VOISINES.
    call resvoi(modele, ma, '&&RESTHER.VOISIN')
!
! BOUCLE SUR LES CHARGEMENTS -----------------------------------------
!
! ATTENTION: POUR UN MEME TYPE DE CHARGEMENT, SEULE LA DERNIERE
! OCCURENCE EST CONSERVEE.
!
    nomgdf = ' '
    nomgdh = ' '
    nomgdt = ' '
    nomgds = ' '
    cartef = ' '
    carteh = ' '
    cartet = ' '
    cartes = ' '
    chsour = ' '
    iretf = 0
    ireth = 0
    irett = 0
    irets = 0
    iretep = 0
! OPTION DE CALCUL PAR DEFAUT
    psourc='PSOURCR'
!
! BOUCLE SUR LES AFFE_CHAR_THER
    do 10 i = 1, nchar
! INIT.
        cartf = lchar(i)//'.CHTH.FLURE'
        carth = lchar(i)//'.CHTH.COEFH'
        cartt = lchar(i)//'.CHTH.T_EXT'
        carts = lchar(i)//'.CHTH.SOURE'
        cartep = lchar(i)//'.CHTH.HECHP'
! DETERMINE L'EXISTENCE DES SD DE TYPE CHAMP_GD ET DE NOM CARTE.
        call exisd('CHAMP_GD', cartf, iretf)
        call exisd('CHAMP_GD', carth, ireth)
        call exisd('CHAMP_GD', cartt, irett)
        call exisd('CHAMP_GD', carts, irets)
        call exisd('CHAMP_GD', cartep, iretep)
        if (iretep .ne. 0) then
            call u2mess('A', 'CALCULEL6_42')
        endif
        if (((ireth.eq.0).and.(irett.ne.0)) .or. ((irett.eq.0).and.( ireth.ne.0))) ASSERT(.false.)
!
! TRAITEMENT DES CHARGEMENTS DE TYPE FLUX_REP/FLUN
        if (iretf .ne. 0) then
! SURCOUCHE DE DISMCA RENVOYANT LE NOM DE LA SD (NOMGD...) VIA UN
! JEVEUO/JENUNO SUR CARTF//'.DESC'.
            call dismoi('F', 'NOM_GD', cartf, 'CARTE', ibid,&
                        nomgdf, ier)
!
! EXTENSION DE LA CARTE CARTEF VIA CARTEF//'.PTMA' ET '.PTMS' SUR 'V'
            call etenca(cartf, ligrel, ier)
            ASSERT(ier.eq.0)
!
! SEULE CARTE FLUN CONSERVEE (REGLE SURCHARGE USUELLE DE LA DERNIERE)
            if (cartef .ne. ' ') then
                call u2mesk('I', 'CALCULEL6_43', 1, 'FLUX LINEAIRE')
                call jedetr(cartef//'.PTMA')
                call jedetr(cartef//'.PTMS')
            endif
            cartef = cartf
        endif
!
! TRAITEMENT DES CHARGEMENTS DE TYPE ECHANGE/COEF_H
        if (ireth .ne. 0) then
            call dismoi('F', 'NOM_GD', carth, 'CARTE', ibid,&
                        nomgdh, ier)
            call etenca(carth, ligrel, ier)
            ASSERT(ier.eq.0)
!
! TRAITEMENT DES CHARGEMENTS DE TYPE ECHANGE/TEMP_EXT
            call dismoi('F', 'NOM_GD', cartt, 'CARTE', ibid,&
                        nomgdt, ier)
            call etenca(cartt, ligrel, ier)
            ASSERT(ier.eq.0)
!
! SEULE CARTE FLUN CONSERVEE (REGLE SURCHARGE USUELLE DE LA DERNIERE)
            if (carteh .ne. ' ') then
                call u2mesk('I', 'CALCULEL6_43', 1, 'ECHANGE')
                call jedetr(carteh//'.PTMA')
                call jedetr(carteh//'.PTMS')
                call jedetr(cartet//'.PTMA')
                call jedetr(cartet//'.PTMS')
            endif
            carteh = carth
            cartet = cartt
        endif
!
! TRAITEMENT DES SOURCES VOLUMIQUES
        if (irets .ne. 0) then
            chsour = carts//'.DESC'
            call dismoi('F', 'NOM_GD', carts, 'CARTE', ibid,&
                        nomgds, ier)
! SEULE CARTE FLUN CONSERVEE (REGLE SURCHARGE USUELLE DE LA DERNIERE)
            if (cartes .ne. ' ') then
                call u2mesk('A', 'CALCULEL6_43', 1, 'SOURCE')
            endif
! OPTION DE CALCUL POUR SOURCE VARIABLE
            if (nomgds(1:6) .eq. 'SOUR_F') psourc='PSOURCF'
            cartes = carts
        endif
!
! FIN BOUCLE AFFE_CHAR_THER
10  end do
!
    call jedema()
!
end subroutine
