subroutine getcon(nomres, iob, ishf, ilng, ctype,&
                  lcon, iadvar, nomob)
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
! aslint: disable=
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/jjvern.h"
#include "asterfort/lxlgut.h"
#include "asterfort/wkvect.h"
    character(len=*) :: nomres
    integer :: ctype, lcon, iob, ishf, ilng
    integer :: iadvar, loc
    character(len=24) :: nomob
! IN  NOMRES  K*  NOM DU CONCEPT DEMANDE
! IN  IBOB    I  POUR UNE COLLECTION : NUMERO DE L OBJET
! IN  ISHF    I  POUR UN VECTEUR     : SHIFT DANS LE VECTEUR
! IN  ILNG    I  POUR UN VECTEUR     : NB DE VALEURS REQUISES APRES ISHF
! OUT CTYPE   I   LE TYPE : CTYPE (1=REEL, 2=ENTIER, 3=COMPLEXE,
!                                  4=K8,5=K16,6=K24,7=K32,8=K80, 9=I4)
!                                  0=PAS DE VALEUR, <0=ERREUR
! OUT LCON    I  LISTE DES LONGUEURS : NOMBRE DE VALEURS
! OUT IADVAR  I  LISTE DES ADRESSES DU TABLEAU
! OUT NOMOB   K* POUR UNE COLLECTION : NOM DE L OBJET SI EXISTE
!
    character(len=8) :: k8bid
    character(len=4) :: type
    character(len=2) :: acces
    character(len=1) :: xous, genr
    integer :: jres, iret, ibid, lobj, iad, kk
    character(len=32) :: noml32
!     ------------------------------------------------------------------
    ctype = -1
!             123456789.123456789.12
    noml32='                      '
    noml32=nomres
!     AU DELA DE 24 : RESERVE JEVEUX &&xxxx
    call assert(lxlgut(noml32).le.24)
    call jjvern(noml32, 0, iret)
    if (iret .eq. 0) then
!     CET OBJET N'EXISTE PAS
        goto 999
    endif
    call jelira(noml32, 'XOUS', ibid, xous)
    call jelira(noml32, 'GENR', ibid, genr)
    nomob=' '
    if (xous .eq. 'X') then
!     ------------------------------------------------------------------
!     CET OBJET EST UNE COLLECTION, ON VEUT SON ELEMENT NUMERO IOB
!     ------------------------------------------------------------------
        ctype=0
        call jeexin(jexnum(noml32, iob), iret)
        if (iret .le. 0) goto 999
        call jelira(noml32, 'ACCES', ibid, acces)
        if (acces .eq. 'NO') then
            call jenuno(jexnum(noml32, iob), nomob)
        endif
        call jeveuo(jexnum(noml32, iob), 'L', jres)
        call jelira(jexnum(noml32, iob), 'LONMAX', lobj, k8bid)
        call jelira(jexnum(noml32, iob), 'TYPELONG', ibid, type)
        lcon = lobj
        if (type .eq. 'R') then
!     LES VALEURS SONT REELLES
            ctype=1
            iadvar=loc(zr(jres))
        else if (type.eq.'I') then
!     LES VALEURS SONT ENTIERES
            ctype=2
            iadvar=loc(zi(jres))
        else if (type.eq.'IS') then
!     LES VALEURS SONT ENTIERES
            ctype=9
            iadvar=loc(zi4(jres))
        else if (type.eq.'C') then
!     LES VALEURS SONT COMPLEXES
            ctype=3
            iadvar=loc(zc(jres))
        else if (type.eq.'K8') then
!     LES VALEURS SONT DES CHAINES
            ctype=4
            iadvar=loc(zk8(jres))
        else if (type.eq.'K16') then
!     LES VALEURS SONT DES CHAINES
            ctype=5
            iadvar=loc(zk16(jres))
        else if (type.eq.'K24') then
!     LES VALEURS SONT DES CHAINES
            ctype=6
            iadvar=loc(zk24(jres))
        else if (type.eq.'K32') then
!     LES VALEURS SONT DES CHAINES
            ctype=7
            iadvar=loc(zk32(jres))
        else if (type.eq.'K80') then
!     LES VALEURS SONT DES CHAINES
            ctype=8
            iadvar=loc(zk80(jres))
        else
!     TYPE INCONNU
            ctype=0
        endif
    else if ((xous.eq.'S').and.(genr.ne.'N')) then
!     ------------------------------------------------------------------
!     CET OBJET EXISTE ET EST SIMPLE. ON PEUT AVOIR SA VALEUR
!     ------------------------------------------------------------------
        call jeveuo(noml32, 'L', jres)
!          CALL JELIRA(NOML32,'LONUTI',LCON,K8BID)
!          IF (LCON.EQ.0) THEN
!              CALL JELIRA(NOML32,'LONMAX',LCON,K8BID)
!          ENDIF
        call jelira(noml32, 'LONMAX', lcon, k8bid)
        if (ilng .ne. 0) lcon=ilng
        call jelira(noml32, 'TYPELONG', ibid, type)
        if (type .eq. 'R') then
!     LES VALEURS SONT REELLES
            ctype=1
            iadvar=loc(zr(jres+ishf))
        else if (type.eq.'I') then
!     LES VALEURS SONT ENTIERES
            ctype=2
            iadvar=loc(zi(jres+ishf))
        else if (type.eq.'S') then
!     LES VALEURS SONT ENTIERES
            ctype=9
            iadvar=loc(zi4(jres+ishf))
        else if (type.eq.'C') then
!     LES VALEURS SONT COMPLEXES
            ctype=3
            iadvar=loc(zc(jres+ishf))
        else if (type.eq.'K8') then
!     LES VALEURS SONT DES CHAINES
            ctype=4
            iadvar=loc(zk8(jres+ishf))
        else if (type.eq.'K16') then
!     LES VALEURS SONT DES CHAINES
            ctype=5
            iadvar=loc(zk16(jres+ishf))
        else if (type.eq.'K24') then
!     LES VALEURS SONT DES CHAINES
            ctype=6
            iadvar=loc(zk24(jres+ishf))
        else if (type.eq.'K32') then
!     LES VALEURS SONT DES CHAINES
            ctype=7
            iadvar=loc(zk32(jres+ishf))
        else if (type.eq.'K80') then
!     LES VALEURS SONT DES CHAINES
            ctype=8
            iadvar=loc(zk80(jres+ishf))
        else
!     TYPE INCONNU
            ctype=0
        endif
    else if ((xous.eq.'S').and.(genr.eq.'N')) then
!     ------------------------------------------------------------------
!     CET OBJET EST SIMPLE MAIS C EST UN REPERTOIRE DE NOMS
!     ------------------------------------------------------------------
!          CALL JELIRA(NOML32,'NOMUTI',LCON,K8BID)
!          IF (LCON.EQ.0) THEN
!              CALL JELIRA(NOML32,'NOMMAX',LCON,K8BID)
!          ENDIF
        call jelira(noml32, 'NOMMAX', lcon, k8bid)
        call jelira(noml32, 'TYPELONG', ibid, type)
        call jedetr('&&GETCON.PTEUR_NOM')
        call wkvect('&&GETCON.PTEUR_NOM', 'V V '//type, lcon, iad)
        if (type .eq. 'K8') then
            do 51, kk=1,lcon
            call jenuno(jexnum(noml32, kk), zk8(iad-1+kk))
51          continue
            ctype=4
            iadvar=loc(zk8(iad))
        else if (type.eq.'K16') then
            do 52, kk=1,lcon
            call jenuno(jexnum(noml32, kk), zk16(iad-1+kk))
52          continue
            ctype=5
            iadvar=loc(zk16(iad))
        else if (type.eq.'K24') then
            do 53, kk=1,lcon
            call jenuno(jexnum(noml32, kk), zk24(iad-1+kk))
53          continue
            ctype=6
            iadvar=loc(zk24(iad))
        endif
    endif
!
999  continue
end subroutine
