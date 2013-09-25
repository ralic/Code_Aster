subroutine stkmai(ifl, icl, iv, rv, cv,&
                  cnl, mcl, nbm, nume, numn,&
                  cnx, typ, fmt, irteti)
    implicit none
!       ----------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!       SECONDE LECTURE DES DONNEES POUR UN MOT CLE DE TYPE MAILLE
!       ----------------------------------------------------------------
!       IN      IFL,ICL,IV,RV,CV,CNL = VOIR LIRITM
!               MCL             = MOTS CLES TYPE MAILLE
!               NBM             = NB DE MOTS CLES TYPE MAILLE
!               FMT             = NB NOEUDS A LIRE / MAILLE
!               CNX             = NOMU.CONXV
!               TYP             = NOMU.TYPMAIL ASSOCIE A NOMU.NOMMAI
!               NUME            = NUMERO DE L ELEMENT COURANT
!               NUMN            = NUMERO DU NOEUD COURANT DANS CNX
!       OUT     (RETURN)        = MOT CLE SUIVANT (MOT CLE NON RECONNU)
!               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
!               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE
!                                                  OU ERREUR DETECTE)
!       ----------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/liritm.h"
#include "asterfort/lirtet.h"
#include "asterfort/tesfin.h"
#include "asterfort/tesmcl.h"
#include "asterfort/utmess.h"
!
    real(kind=8) :: rv
    integer :: nbm
    character(len=8) :: mcl(nbm), noma, b8
    integer :: deblig, fmt(nbm)
    character(len=14) :: cnl
    character(len=*) :: cv
    character(len=24) :: cnx, typ, nom
    save b8
!
!-----------------------------------------------------------------------
    integer :: i, iadc, iadt, icl, ifl, iret
    integer :: irtet, irteti, iv, nume, numn
    integer :: numtcl
!-----------------------------------------------------------------------
    data b8         /'        '/
    call jemarq()
    irteti = 0
!
!
! - ITEM = MOT CLE  TYPE MAILLE ?
!
    do i = 1, nbm
        call tesmcl(icl, iv, cv, mcl(i), irtet)
        if (irtet .eq. 1) goto 4
        numtcl = i
        goto 5
  4     continue
    end do
    goto 3
!
!
  5 continue
    call jeveuo(cnx, 'E', iadc)
    call jeveuo(typ, 'E', iadt)
!
! - LECTURE DE L'ENTETE
!
    deblig=0
    call lirtet(ifl, 2, 0, cnl, nom,&
                icl, iv, rv, cv, deblig)
    goto 9
!
! - LIRE ITEM SUIVANT = NOM DE MAILLE ?
!
  7 continue
    call liritm(ifl, icl, iv, rv, cv,&
                cnl, deblig, 2)
  9 continue
!
! - ITEM = MOT  CLE FIN  OU FINSF ?
!
    call tesfin(icl, iv, cv, irtet)
    if (irtet .eq. 1) then
        goto 1
    else if (irtet .eq. 2) then
        goto 2
    endif
!
! - CREATION DE CONXV.NOM_DE_MAILLE ET TYPMAIL.NOM_DE_MAILLE
!
    noma = b8
    noma(1:iv) = cv(1:iv)
    call jeexin(jexnom(typ(1:8)//'.NOMMAI', noma), iret)
    if (iret .eq. 0) then
        call jecroc(jexnom(typ(1:8)//'.NOMMAI', noma))
        call jecroc(jexnom(cnx, noma))
        call jeecra(jexnom(cnx, noma), 'LONMAX', fmt(numtcl))
    else
        call utmess('F', 'MODELISA7_10', sk=noma)
    endif
!
! - STOCKAGE DES NOMS DES NOEUDS DE LA MAILLE ET DU TYPE DE MAILLE
!
    zi(iadt+nume) = numtcl
!
    do i = 1, fmt(numtcl)
        call liritm(ifl, icl, iv, rv, cv,&
                    cnl, deblig, 2)
        nom = b8
        nom(1:iv) = cv(1:iv)
!
        zk8(iadc+numn) = nom(1:8)
!
! - INCREMENTATION DU NB DE NOEUDS LUS
!
        numn = numn + 1
    end do
!
! - INCREMENTATION DU NB D ELEMENTS LUS
!
    nume = nume + 1
!
! - MAILLE SUIVANTE
!
    goto 7
!
  1 continue
    irteti = 1
    goto 999
  2 continue
    irteti = 2
    goto 999
  3 continue
    irteti = 0
    goto 999
!
999 continue
    call jedema()
end subroutine
