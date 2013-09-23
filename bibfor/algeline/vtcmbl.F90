subroutine vtcmbl(nbcmb, typcst, const, typech, nomch,&
                  typres, chpres)
!     ------------------------------------------------------------------
!     COMBINAISON LINEAIRE DE CHAM_NO OU DE CHAM_ELEM
!     ------------------------------------------------------------------
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
!     -----------------------------------------------------------------
!     *  LES CHAM_NOS OU CHAM_ELEMS SONT REELS OU COMPLEXES
!     *  LES SCALAIRES SONT REELS OU COMPLEXES
!     -----------------------------------------------------------------
! IN  : NBCOMB : IS  : NOMBRE DE CHAM_GDS A COMBINER
! IN  : TYPCST : K1  : TYPE DES CONSTANTES (R OU C, OU I )
! IN  : CONST  : R8  : TABLEAU DES COEFFICIENTS
! IN  : TYPECH : K1  : TYPE DES CHAM_GDS   (R OU C)
! IN  : NOMCH  : K19 : NOMS DES CHAM_GDS
! IN  : TYPRES : K1  : TYPE DU CHAMP RESULTAT (R OU C)
! IN  : CHPRES : K19 : NOM DU CHAMP RESULTAT
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/gcncon.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/sdchgd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: nbcmb
    real(kind=8) :: const(*)
    character(len=*) :: typcst(*), typech(*), nomch(*), typres, chpres
!
!
! DECLARATION VARIABLES LOCALES
    integer :: ibid, irefe, idime, i, ifm, niv
    integer :: icmb, iret, ival, ilimpi, lvale, iconst
    integer :: jdesc, jrefe, jvale
    integer :: kdesc, krefe, kvale
    integer :: nbdesc, nbrefe, nbvale
    integer :: nbdes1, nbref1, nbval1
    real(kind=8) :: dimag
    complex(kind=8) :: c8cst
    character(len=4) :: docu, type
    character(len=5) :: refe, desc, vale
    character(len=8) :: k8b
    character(len=19) :: ch19, ch19r
    character(len=24) :: k24b
!     ------------------------------------------------------------------
!
    call jemarq()
! RECUPERATION ET MAJ DU NIVEAU D'IMPRESSION
    call infniv(ifm, niv)
!
!-----------------------------------------------------------------------
! --- PHASE D'INITIALISATION
!-----------------------------------------------------------------------
    type=typres
    ch19=nomch(1)
!
! CHAM_NO OU CHAM_ELEM ?
    k24b=ch19//'.DESC'
    call jeexin(k24b, ibid)
    if (ibid .gt. 0) then
        k24b=ch19//'.DESC'
        call jelira(k24b, 'DOCU', cval=docu)
    else
        k24b=ch19//'.CELD'
        call jelira(k24b, 'DOCU', cval=docu)
    endif
!
!
! INIT. DE BASE
    if (docu .eq. 'CHNO') then
        refe='.REFE'
        desc='.DESC'
        vale='.VALE'
    else if (docu.eq.'CHML') then
        refe='.CELK'
        desc='.CELD'
        vale='.CELV'
    else
        call utmess('F', 'UTILITAI_21')
    endif
!
!
!
!   PREMIER CHAM_NO A CONCATENER
    ch19=nomch(1)
!
!   OBTENTION DES ADRESSES ET DES TAILLES DES .DESC, .REFE ET .VALE
!   DU PREMIER CHAM_NO A CONCATENER. ON SUPPOSE QUE
!   TOUS LES CHAM_NOS DE LA LISTE NOMCH SONT HOMOGENES SUR CE POINT.
    call jelira(ch19//desc, 'LONMAX', nbdesc)
    call jelira(ch19//vale, 'LONMAX', nbvale)
    call jelira(ch19//refe, 'LONMAX', nbrefe)
    call jeveuo(ch19//desc, 'L', jdesc)
    call jeveuo(ch19//refe, 'L', jrefe)

!   CONSTRUCTION D'UN CHAM_GD RESULTAT SUR LE MODELE DE NOMCH(1)
    ch19r=chpres
    call jeexin(ch19r//vale, iret)
    if (iret .eq. 0) then
        call wkvect(ch19r//desc, 'V V I', nbdesc, kdesc)
        call wkvect(ch19r//vale, 'V V '//type, nbvale, kvale)
        call wkvect(ch19r//refe, 'V V K24', nbrefe, krefe)
    else
        call jeveuo(ch19r//desc, 'E', kdesc)
        call jelira(ch19r//desc, 'LONMAX', nbdes1)
        call jeveuo(ch19r//refe, 'E', krefe)
        call jelira(ch19r//refe, 'LONMAX', nbref1)
        call jelira(ch19r//vale, 'LONMAX', nbval1)
!       VERIFICATION DE LA COHERENCE DES DIMENSIONS
        ASSERT(nbdes1.eq.nbdesc)
        ASSERT(nbref1.eq.nbrefe)
        ASSERT(nbval1.eq.nbvale)
    endif

    call jeecra(ch19r//desc, 'DOCU', cval=docu)
!   RECOPIE DU .DESC ET DU .REFE DU PREMIER CHAM_NO DE LA LISTE
!   DANS CEUX DU CHAM_NO SOLUTION
    do 30 i = 0, nbdesc-1
        zi(kdesc+i)=zi(jdesc+i)
30  continue
    do 40 i = 0, nbrefe-1
        zk24(krefe+i)=zk24(jrefe+i)
40  continue
!
!   CHANGER LA GRANDEUR
    call sdchgd(ch19r, typres)
!
!   VECTEUR RECEPTACLE TEMPORAIRE DE LA COMBINAISON LINEAIRE
    call wkvect('&&VTCMBL.VALE', 'V V '//type, nbvale, lvale)
!
!
!-----------------------------------------------------------------------
! --- BOUCLE SUR LES CHAM_GDS A COMBINER
!-----------------------------------------------------------------------
    iconst=1
    do 120 icmb = 1, nbcmb
!
!       CHAM_NO A CONCATENER
        ch19=nomch(icmb)
!
        call jeveuo(ch19//vale, 'L', jvale)
        if (typres(1:1) .eq. 'R') then
            if (typech(icmb)(1:1) .eq. 'R') then
                do 50 ival = 0, nbvale-1
                    zr(lvale+ival)=zr(lvale+ival)+ const(&
                            iconst)*zr(jvale+ival)
50              continue
            else
                if (typcst(icmb)(1:1) .eq. 'R') then
                    do 60 ival = 0, nbvale-1
                        zr(lvale+ival)=zr(lvale+ival)+&
                                const(iconst)*dble(zc(jvale+ival))
60                  continue
                else if (typcst(icmb)(1:1).eq.'I') then
                    do 70 ival = 0, nbvale-1
                        zr(lvale+ival)=zr(lvale+ival)+&
                                const(iconst)*dimag(zc(jvale+ival))
70                  continue
                else
                    type=typcst(icmb)(1:1)
                    call utmess('F', 'PREPOST3_6', sk=type)
                endif
            endif
        else
            if (typech(icmb)(1:1) .eq. 'R') then
                if (typcst(icmb)(1:1) .eq. 'R') then
                    do 80 ival = 0, nbvale-1
                        zc(lvale+ival)=zc(lvale+ival)+&
                                const(iconst)*zr(jvale+ival)
80                  continue
                else if (typcst(icmb)(1:1).eq.'C') then
                    c8cst=dcmplx(const(iconst),const(iconst+1)&
                            )
                    do 90 ival = 0, nbvale-1
                        zc(lvale+ival)=zc(lvale+ival)+c8cst*&
                                zr(jvale+ival)
90                  continue
                endif
            else
                if (typcst(icmb)(1:1) .eq. 'R') then
                    do 100 ival = 0, nbvale-1
                        zc(lvale+ival)=zc(lvale+ival)+&
                                const(iconst)*zc(jvale+ival)
100                  continue
                else if (typcst(icmb)(:1).eq.'C') then
                    c8cst=dcmplx(const(iconst),const(iconst+1)&
                            )
                    do 110 ival = 0, nbvale-1
                        zc(lvale+ival)=zc(lvale+ival)+c8cst*&
                                zc(jvale+ival)
110                  continue
                endif
            endif
        endif
        call jelibe(ch19//vale)
        iconst=iconst+1
        if (typcst(icmb)(1:1) .eq. 'C') iconst=iconst+1
120  continue

!
!   IL EST NECESSAIRE D'ACTUALISER KVALE SI LE RESULTAT EST DANS NOMCH()
    call jeveuo(ch19r//vale, 'E', kvale)
    if (type(1:1) .eq. 'R') then
        do 130 ival = 0, nbvale-1
            zr(kvale+ival)=zr(lvale+ival)
130      continue
    else if (type(1:1).eq.'C') then
        do 140 ival = 0, nbvale-1
            zc(kvale+ival)=zc(lvale+ival)
140      continue
    endif
!
    call jedetr('&&VTCMBL.VALE')
    call jelibe(ch19r//vale)
!
    call jedema()
end subroutine
