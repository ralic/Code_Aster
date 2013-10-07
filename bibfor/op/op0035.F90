subroutine op0035()
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
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
!
!
! DECLARATION VARIABLES LOCALES
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/uttrst.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/tbajco.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/titre.h"
#include "asterfort/ulisog.h"
#include "asterfort/ulnomf.h"
#include "asterfort/ulnume.h"
#include "asterfort/wkvect.h"
    character(len=1) :: ketat
    character(len=8) :: result, k8bid, etat
    character(len=16) :: type, nomcmd
    character(len=255) :: kfic
    complex(kind=8) :: cbid
    integer :: nbval, jkval, jtval, ibid, k, ul
    integer :: nbunit, nbfic
    real(kind=8) :: rval, rbid
!
    call jemarq()
    call infmaj()
    call getres(result, type, nomcmd)
    ketat='?'
!
!=======================================================================
!- NOM DES PARAMETRES REELS A RECUPERER
!=======================================================================
!
    call getvtx(' ', 'LISTE_INFO', nbval=0, nbret=nbval)
    nbval=-nbval
    call wkvect('&&LISTE_INFO', 'V V K16', nbval, jkval)
    call wkvect('&&TYPE_INFO', 'V V K8', nbval, jtval)
!
    call getvtx(' ', 'LISTE_INFO', nbval=nbval, vect=zk16(jkval), nbret=ibid)
    do 20 k = 1, nbval
        if (zk16(jkval+k-1) .eq. 'CPU_RESTANT') then
            zk8(jtval+k-1) = 'R'
        else if (zk16(jkval+k-1) .eq. 'UNITE_LIBRE') then
            zk8(jtval+k-1) = 'I'
        else if (zk16(jkval+k-1) .eq. 'ETAT_UNITE') then
            zk8(jtval+k-1) = 'K8'
        endif
20  end do
!
    call tbcrsd(result, 'G')
    call tbajpa(result, nbval, zk16(jkval), zk8(jtval))
!
    do 100 k = 1, nbval
        if (zk16(jkval+k-1) .eq. 'CPU_RESTANT') then
!         -- TEMPS CPU RESTANT :
            call uttrst(rval)
            call tbajli(result, nbval, zk16(jkval+k-1), [ibid], [rval],&
                        [cbid], k8bid, 0)
        else if (zk16(jkval+k-1) .eq. 'UNITE_LIBRE') then
            ul = ulnume ()
            call tbajli(result, nbval, zk16(jkval+k-1), [ul], [rbid],&
                        [cbid], k8bid, 0)
        else if (zk16(jkval+k-1) .eq. 'ETAT_UNITE') then
            call getvis(' ', 'UNITE', scal=ul, nbret=nbunit)
            if (nbunit .eq. 0) then
                call getvtx(' ', 'FICHIER', scal=kfic, nbret=nbfic)
                ul = ulnomf (kfic, k8bid, k8bid)
            endif
            etat='FERME  '
            if (ul .ge. 0) then
                call ulisog(ul, kfic, ketat)
                if (ketat .eq. 'O') then
                    etat='OUVERT  '
                else if (ketat .eq. 'R') then
                    etat='RESERVE '
                endif
            endif
            call tbajli(result, nbval, zk16(jkval+k-1), [ibid], [rbid],&
                        [cbid], etat, 0)
            if ((ketat .eq. 'R') .or. (ketat .eq. 'O')) then
!       SI LE FICHIER EST RESERVE OU OUVERT LE NOM EST MIS DANS LA TABLE
!             K*255 = 4*K*80 = (1,80)+(81,160)+(161,240)+(241,255)
!                                 80      80        80        15
!           5 COLONNES SONT AJOUTÉES A LA LIGNE K DE LA TABLE
                call tbajco(result, 'NOMFIC1', 'K80', 1, [ibid],&
                            [rbid], [cbid], kfic( 1: 80), 'A', [k])
                call tbajco(result, 'NOMFIC2', 'K80', 1, [ibid],&
                            [rbid], [cbid], kfic( 81:160), 'A', [k])
                call tbajco(result, 'NOMFIC3', 'K80', 1, [ibid],&
                            [rbid], [cbid], kfic(161:240), 'A', [k])
                call tbajco(result, 'NOMFIC4', 'K80', 1, [ibid],&
                            [rbid], [cbid], kfic(241:255), 'A', [k])
            endif
        endif
100  end do
!
    call titre()
!
    call jedema()
end subroutine
