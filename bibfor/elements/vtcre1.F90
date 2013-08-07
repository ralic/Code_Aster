subroutine vtcre1(champ, numedd, classe, type, method,&
                  sdfeti, neq)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  CREATION DES OBJETS JEVEUX DE LA STRUCTURE
!      DE DONNEES CHAMNO. UTILITAIRE DE BAS NIVEAU LANCE PAR VTCREB.
!
!     IN  CHAMP  : K24 : NOM DU CHAM_NO A CREER
!     IN  NUMEDD : K24 : PROF_CHNO DU CHAM_NO
!     IN  CLASSE : K1  : NOM DE LA BASE SUR LAQUELLE LE CHAM_NO DOIT
!                        ETRE CREE
!     IN  TYPCE  : K1  : TYPE DES VALEURS DU CHAM_NO A CREER
!                  'R'    ==> COEFFICIENTS REELS
!                  'C'    ==> COEFFICIENTS COMPLEXES
!     OUT METHOD,SDFETI: K24 : PARAMETRES DU SOLVEUR.
!     OUT   NEQ   : I   : INTEGER
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES:
!       DIVERS: DISMOI,SDCHGD,UTIMSD.
!       JEVEUX:JEMARQ,JEDEMA,WKVECT,JEVEUO,JEECRA.
!
!     FONCTIONS INTRINSEQUES:
!       NONE.
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       25/11/03 (OB): CREATION.
!----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/sdchgd.h"
#include "asterfort/wkvect.h"
    character(len=1) :: classe, type
    character(len=24) :: champ, numedd, method, sdfeti
    integer :: neq
!
!
! DECLARATION VARIABLES LOCALES
    integer :: jchamp, jrefn, ierd, jneq, lchp
    character(len=8) :: k8bid
    character(len=24) :: vale, refe, desc
!
    data vale/'                   .VALE'/
    data refe/'                   .REFE'/
    data desc/'                   .DESC'/
!
!------------------------------------------------------------------
    call jemarq()
!
! ------------------------------- REFE --------------------------------
! --- AFFECTATION DES INFORMATIONS DE REFERENCE A CHAMP
!
! ON ETOFFE L'OBJET .REFE DU CHAMNO AVEC LE TYPE DE RESOLUTION ET LE
! NOM DE LA SD_FETI PRIS DANS LE .REFN DU NUME_DDL SOUS-JACENT
!
    call jeveuo(numedd(1:14)//'.NUME.REFN', 'L', jrefn)
    refe(1:19) = champ
!
    method = zk24(jrefn+2)
    sdfeti = zk24(jrefn+3)
    call wkvect(refe, classe//' V K24', 4, jchamp)
    zk24(jchamp) = zk24(jrefn)
    zk24(jchamp+1) = numedd(1:14)//'.NUME'
    if (method .eq. 'FETI') then
        zk24(jchamp+2) = method
        zk24(jchamp+3) = sdfeti
    endif
!
! ------------------------------- DESC --------------------------------
! --- AFFECTATION DES INFORMATIONS DE REFERENCE A CHAMP
!
    desc(1:19) = champ
    call wkvect(desc, classe//' V I', 2, jchamp)
    call jeecra(desc, 'DOCU', cval='CHNO')
    call dismoi('F', 'NUM_GD_SI', numedd, 'NUME_DDL', zi(jchamp),&
                k8bid, ierd)
    zi(jchamp+1) = 1
!
! ------------------------------- VALE --------------------------------
! --- CREATION DE L'OBJET SIMPLE DES VALEURS
! --- TYPE DES VALEURS, LONGUEUR D'UN VECTEUR
!
    call jeveuo(numedd(1:14)//'.NUME.NEQU', 'L', jneq)
    neq = zi(jneq)
    vale(1:19) = champ
    call jecreo(vale, classe//' V '//type)
    call jeecra(vale, 'LONMAX', neq)
    call jeveuo(vale, 'E', lchp)
!
! --- CHANGER LA GRANDEUR
    call sdchgd(champ, type)
!
! FIN ------------------------------------------------------
    call jedema()
end subroutine
