subroutine crchno(champ, prno, gran, noma, base,&
                  typc, nbnoeu, lonval)
    implicit none
#include "jeveux.h"
!
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jexnom.h"
#include "asterfort/wkvect.h"
    character(len=*) :: champ, prno, gran, noma, base, typc
    integer :: nbnoeu, lonval
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
! ----------------------------------------------------------------------
!     CREATION D'UNE STRUCTURE CHAMNO "CHAMP"
! IN  CHAMP  : CH8 : NOM DU CHAMNO A CREER
! IN  PRNO   : CH24: NOM DU PROFCHNO ASSOCIE
! IN  GRAN   : CH8 : NOM DE LA GRANDEUR ASSOCIEE
! IN  NOMA   : CH8 : NOM DU MAILLAGE ASSOCIE AU CHAMNO
! IN  BASE   : CH1 : NOM DE LA BASE SUR LAQUELLE LE CHAM_NO DOIT ETRE
!                    CREE
! IN  TYPC   : CH1 : TYPE DES VALEURS DU CHAMNO A CREER
!              'R'  ==> COEFFICIENTS REELS
!              'C'  ==> COEFFICIENTS COMPLEXES
!              'K..'==> COEFFICIENTS K..
! IN  NBNOEU : I   : NOMBRE DE NOEUDS DU MAILLAGE ASSOCIE AU CHAMNO
! IN  LONVAL : I   : DIMENSION DU .VALE
!     ------------------------------------------------------------------
!     PRECAUTIONS D'EMPLOI :
!       1) LE CHAMNO "CHAMP" NE DOIT PAS EXISTER
!       2) LES COEFFICIENTS DU CHAMNO "CHAMP" NE SONT PAS AFFECTES
! ----------------------------------------------------------------------
!
!
!
!
    integer :: nbval, lchamp
    character(len=1) :: classe
    character(len=3) :: type
    character(len=8) :: cbid
    character(len=24) :: vale, refe, desc
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    data  vale  /'                   .VALE'/
    data  refe  /'                   .REFE'/
    data  desc  /'                   .DESC'/
!     ------------------------------------------------------------------
    call jemarq()
    classe = base(1:1)
!
!     --------------------------- REFE --------------------------------
!     --- AFFECTATION DES INFORMATIONS DE REFERENCE A CHAMP ---
    nbval = 4
    refe(1:19) = champ
    call wkvect(refe, classe//' V K24', nbval, lchamp)
    zk24(lchamp) = noma
    zk24(lchamp+1) = prno
!
!     --------------------------- DESC --------------------------------
!     --- AFFECTATION DES INFORMATIONS DE REFERENCE A CHAMP ---
    nbval = 2
    desc(1:19) = champ
    call wkvect(desc, classe//' V I', nbval, lchamp)
    call jeecra(desc, 'DOCU', nbval, 'CHNO')
    call jenonu(jexnom('&CATA.GD.NOMGD', gran), zi(lchamp))
    zi(lchamp+1) = nbnoeu
!
!     --------------------------- VALE --------------------------------
!     ------------- CREATION DE L'OBJET SIMPLE DES VALEURS -------------
!     --- TYPE DES VALEURS, LONGUEUR D'UN VECTEUR ---
!
    vale(1:19) = champ
    type = typc
    call jecreo(vale, classe//' V '//type)
    call jeecra(vale, 'LONMAX', lonval, cbid)
!
    call jedema()
end subroutine
