subroutine op0139()
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: patrick.massin at edf.fr
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/cncinv.h"
#include "asterfort/cnocns.h"
#include "asterfort/dismoi.h"
#include "asterc/getres.h"
#include "asterfort/getvid.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rslesd.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xchavi.h"
#include "asterfort/xdetfo.h"
#include "asterfort/xptfon.h"
#include "asterfort/rs_getfirst.h"
#include "jeveux.h"
    character(len=8) :: nomo, nomfis, resuco, noma
    integer :: ndim
    character(len=19) :: cnsln, cnslt
    character(len=24) :: vvit, vbeta, vitemp
!
! AJOUT OP
!
!     JEVEUX AND GENERAL PURPOSE
    integer :: ibid, ifm, niv, vali(2), i
    real(kind=8) :: vale(3)
    character(len=8) :: k8b
!
!     INPUT DATA
    integer :: nfiss, jfiss
    character(len=8) :: table
!
!     SINGLE
    integer :: jnfon, nfon, piece, jffis, ni, nf
    integer :: nbptff, actpoi, maxact
!
!     POPAGATION DATA
    integer :: jvit, jbeta, iadrma
    integer :: jvitem
!
!     OPTION NB_POINT_FOND
    integer :: sifval
!
!     OPTION COMP_LINE
!     AJOUTS
    character(len=8) :: k8bid
    character(len=16) :: typdis, k16bid
    character(len=19) :: k19b
    character(len=19) :: cnsdet, cnxinv
    character(len=19) :: listpt
    character(len=24) :: mater
    integer :: nume_first
    integer :: jbasc, jfono, jmafon
    integer ::  nmafon, nxptff
    integer :: nfonn, npara
    aster_logical :: lbid
    complex(kind=8) :: c16b(1)
    character(len=12) :: nopar3(5)
    character(len=16) :: operation(1)
    character(len=1) :: typar3(5)
    data nopar3 /'NUME_FOND','NUM_PT','ABSC_CURV', 'VIT', 'BETA'/
    data operation /'FRONT_COHE_BRUT'/
    data typar3 /'I' ,'I','R','R','R'/
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
!   ON RECUPERE LE NOM DE LA FISSURE A PROPAGER
    call getvid(' ', 'FISSURE', scal=nomfis, nbret=ibid)
!
!   VERIFICATION QUE L ON TRAITE UNE FISSURE DE TYPE 'COHESIF'
    call dismoi('TYPE_DISCONTINUITE', nomfis, 'FISS_XFEM', repk=typdis)
    if (typdis .ne. 'COHESIF') call utmess('F', 'XFEM2_1')
    call getvid(' ', 'RESULTAT', scal=resuco, nbret=ibid)
!
!   RECUPERATION DU PREMIER NUMERO D'ORDRE
    call rs_getfirst(resuco, nume_first)
    k8b = 'CARACTER'
!
!   RECUPERATION DU NOM DU MODELE
    call rslesd(resuco, nume_first, nomo, mater, k8b,&
                k19b, ibid)
!
!   RECUPERATION DU MAILLAGE ASSOCIE AU MODELE
    call jeveuo(nomo(1:8)//'.MODELE    .LGRF', 'L', iadrma)
    noma = zk8(iadrma)
!
!   DIMENSION DU PROBLEME
    call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)
    if ((ndim.lt.2) .or. (ndim.gt.3)) then
        call utmess('F', 'XFEM_18')
    endif
!
!   NOM DU CONCEPT TABLE ET CREATION DE LA TABLE
    call getres(table, k16bid, k16bid)
    call tbcrsd(table, 'G')
    npara = 5
    call tbajpa(table, npara, nopar3, typar3)
!
!   RECUPERATION DES LEVEL SETS ET GRADIENTS
    cnslt = '&&OP0010.CNSLT'
    cnsln = '&&OP0010.CNSLN'
    call cnocns(nomfis//'.LTNO', 'V', cnslt)
    call cnocns(nomfis//'.LNNO', 'V', cnsln)
!
    call dismoi('NB_FISS_XFEM', nomo, 'MODELE', repi=nfiss)
    if (nfiss .eq. 0) call utmess('F', 'XFEM2_93', sk=nomo)
!
!   LISTE DES FISSURES CONTENUES DANS LE MODELE
    call jeveuo(nomo//'.FISS', 'L', jfiss)
!
!   CALCUL ANALOGUE DE LST A PARTIR DES VARIABLES INTERNES
!   IL S AGIT DU CHAMP CNSDET
!
    cnsdet = '&&OP0049.CNSDET'
    call xdetfo(cnsdet, cnsln, cnslt, ndim,&
                nmafon, noma, nomfis, resuco)
    call jeveuo('&&XDETFO.MAFOND', 'L',jmafon)
!
! --- PREPARATION DETERMINATION DES POINTS DU FRONT
!
    nxptff = 2*nmafon
    call wkvect('&&OP0049.FONFIS', 'V V R', 11*nxptff, jfono)
    listpt = '&&OP0049.LISTPT'
!
!   CONNECTIVITE INVERSE
    cnxinv = '&&OP0049.CNCINV'
    call cncinv(noma, [0], 0, 'V', cnxinv)
!
! --- APPEL "ALLEGE" A XPTFON
! --- DETERMINATION PTS DESORDONNES DU PREMIER FOND DETECTE
!
    call xptfon(noma, ndim, nmafon, cnsdet, cnsln,&
                cnxinv, jmafon, nxptff, jfono, nfonn,&
                ibid, ibid, nomfis, .false._1, listpt, lbid, typdis,&
                ibid, operation_opt=operation(1))
!
! --- PAS DORDONNANCEMENT, ON REGARDE LE NUAGE DE POINTS UNIQUEMENT
! --- PAS D APPEL A XORIFF
!
!
! --- RECUP INFOS SUR LE FOND DE FISSURE ORIGINEL
! --- A PARTIR DUQUEL ON VA CALCULER LA VITESSE A POSTERORI
!
    call dismoi('NB_FOND', nomfis, 'FISS_XFEM', repi=nfon)
!
    call jeveuo(nomfis//'.FONDMULT', 'L', jnfon)
    call jeveuo(nomfis//'.FONDFISS', 'L', jffis)
    call jeveuo(nomfis//'.BASEFOND', 'L', jbasc)
!
    call dismoi('NB_POINT_FOND', nomfis, 'FISS_XFEM', repi=nbptff)
!
    maxact = nbptff
    actpoi=0
!
! --- BOUCLE SUR LES MORCEAUX DE L ANCIEN FRONT
!
    do piece = 1, nfon
!
!       RECUP INFOS ANCIEN FRONT
        ni=zi(jnfon-1+2*(piece-1)+1)
        nf=zi(jnfon-1+2*(piece-1)+2)
        sifval=nf-ni+1
        vvit  = '&&OP0049.VITESSE'
        vbeta = '&&OP0049.BETA'
        vitemp = '&&OP0049.VITEMP'
        call wkvect(vvit, 'V V R8', sifval, jvit)
        call wkvect(vbeta, 'V V R8', sifval, jbeta)
        call wkvect(vitemp, 'V V R8', sifval, jvitem)
!
!       CALCUL DE LA VITESSE A POSTERIORI
        call xchavi(actpoi, jbasc, jffis, jfono, jvitem,&
                    jbeta, ndim, nfonn, sifval)
!       LES DEUX VALEURS EXTREMITES SEMBLENT FAUSSES, ON LES CORRIGE
        zr(jvitem) = zr(jvitem+1)
        zr(jvitem-1+sifval) = zr(jvitem-2+sifval)

!       ICI, ON POURRA AJOUTER UNE ETAPE DE LISSAGE
!       FONCTION R->R
!
        do i = 1,sifval
            zr(jvit-1+i) = zr(jvitem-1+i)
        end do
!
! STOCKAGE DANS UNE TABLE
!
        do i = 1, sifval
            vali(1) = piece
            vali(2) = i
            vale(1) = zr(jffis-1+4*(actpoi+i-1)+4)
            vale(2) = zr(jvit-1+i)
            vale(3) = zr(jbeta-1+i)
            call tbajli(table, npara, nopar3, vali, vale,&
                        c16b, k8bid, 0)
        end do
        actpoi = actpoi+sifval
        ASSERT(actpoi.le.maxact)
        call jedetr(vvit)
        call jedetr(vbeta)
        call jedetr(vitemp)
!
    end do
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
