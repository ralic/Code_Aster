subroutine cmfiss(main, gno1, gno2, prefix, mainit,&
                  nomgma, noma, connec, tyma, ngma,&
                  gpma)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: mainit
    character(len=8) :: main, prefix
    character(len=24) :: noma, connec, tyma, ngma, gpma, gno1, gno2, nomgma
!
! ----------------------------------------------------------------------
!           CREA_MAILLAGE : CREA_FISS
! ----------------------------------------------------------------------
! IN        MAIN    NOM DU MAILLAGE (INITIAL)
! IN        GNO1    GROUP_NO_1
! IN        GNO2    GROUP_NO_2
! IN        PREFIX  PREFIXE POUR LE NOM DES MAILLES (EX : M, MJ, ...)
! IN        MAINIT  NUMERO INITIAL DES MAILLES CREEES
! IN        NOMGMA  NOM DU GROUP_MA CREE
! IN/JXOUT  NOMA    NOM DES MAILLES CREEES (VECTEUR DE K8)
! IN/JXOUT  CONNEC  CONNECTIVITE DES MAILLES CREEES (LISTE)
!                    NBR DE NOEUDS DE LA MAILLE (ICI, TOUJOURS 4),
!                    NUMEROS DES NDS DE LA MAILLE
! IN/JXOUT  TYMA    TYPE DES MAILLES CREEES (VECTEUR I)
! IN/JXOUT  NGMA    NOM DU GROUP_MA CREE
! IN/JXOUT  GPMA    LISTE DES MAILES DU GROUP_MA CREE
!                    NBR DE MAILLES DU GROUP_MA
!                    NUMERO DES MAILLES (NEGATIF QUAND NOUVELLE MAILLE)
! ----------------------------------------------------------------------
!
!
    integer :: iret, nb1, nb2, nbmajo, tyqua4, ma
    integer :: n1, n2, n3, n4
    integer :: inoma, iconne, ityma, igpma, ingma, jcon, jgno1, jgno2
    integer :: lgma, lgpref
    character(len=80) :: knume
! ----------------------------------------------------------------------
!
!
!
! - VERIFICATIONS DES GROUP_NO
!
    call jeexin(jexnom(main//'.GROUPENO', gno1), iret)
    if (iret .eq. 0) then
        call utmess('F', 'ALGELINE_19', sk=gno1)
    endif
    call jeexin(jexnom(main//'.GROUPENO', gno2), iret)
    if (iret .eq. 0) then
        call utmess('F', 'ALGELINE_19', sk=gno2)
    endif
!
    call jelira(jexnom(main//'.GROUPENO', gno1), 'LONUTI', nb1)
    call jelira(jexnom(main//'.GROUPENO', gno2), 'LONUTI', nb2)
!
    if (nb1 .ne. nb2) then
        call utmess('F', 'ALGELINE_20')
    endif
    if (nb1 .le. 1) then
        call utmess('F', 'ALGELINE_21')
    endif
!
!
! - INITIALISATION
!
    nbmajo = nb1 - 1
    lgpref = lxlgut(prefix)
    call jeveuo(jexnom(main//'.GROUPENO', gno1), 'L', jgno1)
    call jeveuo(jexnom(main//'.GROUPENO', gno2), 'L', jgno2)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'QUAD4'), tyqua4)
!
!
! - CREATION DES VECTEURS RESULTATS
!
    call wkvect(noma, 'V V K8', nbmajo, inoma)
    call wkvect(connec, 'V V I', nbmajo*5, iconne)
    call wkvect(tyma, 'V V I', nbmajo, ityma)
    call wkvect(ngma, 'V V K24', 1, ingma)
    call wkvect(gpma, 'V V I', nbmajo+1, igpma)
!
!
! - NOM ET TAILLE DU NOUVEAU GROUP_MA
!
    zk24(ingma)= nomgma
    zi(igpma) = nbmajo
!
!
! - CREATION DES MAILLES
!
    jcon = iconne
    do 10 ma = 1, nbmajo
!
!      NOM DE LA MAILLE CREEE
        call codent(mainit-1+ma, 'G', knume)
        lgma = lxlgut(knume)
        if (lgma+lgpref .gt. 8) then
            call utmess('F', 'ALGELINE_17')
        endif
        zk8(inoma-1 + ma) = prefix(1:lgpref) // knume
!
!      TYPE DE LA NOUVELLE MAILLE : QUAD4
        zi(ityma-1 + ma) = tyqua4
!
!      CONNECTIVITE DE LA NOUVELLE MAILLE
!
! ATTENTION QUAND ON DEFINIT DEUX GROUPES DE NOEUD ET QUE
! L'ON VEUT CREER UN JOINT GRACE A CES DEUX GROUPES , IL FAUT COMMENCER
! LA RENUMEROTATION  (NOEUD_ORDO)  DANS LE BON SENS DE FACON A CE QUE
! LA CONNECTIVITE LOCALE DEFINIE CI-APRES DONNE DES ED NUMEROTÉ DANS LE
! SENS TRIGO
!
        zi(jcon ) = 4
        n1 = zi(jgno1-1 + ma )
        n2 = zi(jgno1-1 + ma+1)
        n3 = zi(jgno2-1 + ma+1)
        n4 = zi(jgno2-1 + ma )
        zi(jcon+1) = n1
        zi(jcon+2) = n2
        zi(jcon+3) = n3
        zi(jcon+4) = n4
        jcon = jcon + 5
!
! LES NOEUDS TOPOLOGIQUEMENT CONFONDUS NE SERONT PAS BIEN TRAITES
! PAR CALCUL LORS DE LA PHASE DE RESOLUTION NON LINEAIRE
! ON ARRETE DONC DES CE STADE
        if (n1 .eq. n2 .or. n1 .eq. n3 .or. n1 .eq. n4 .or. n2 .eq. n3 .or. n2 .eq. n4 .or.&
            n3 .eq. n4) then
            call utmess('F', 'ALGELINE_22')
        endif
!      INSERTION DE LA MAILLE DANS LE NOUVEAU GROUP_MA
        zi(igpma + ma) = - ma
!
10  end do
!
end subroutine
