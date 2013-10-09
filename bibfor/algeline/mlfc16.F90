subroutine mlfc16(nommat, npivot, neq, typsym, eps,&
                  renumz)
    implicit none
#include "jeveux.h"
#include "asterc/llbloc.h"
#include "asterc/mlnbpr.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedisp.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mlnmin.h"
#include "asterfort/mltasc.h"
#include "asterfort/mltcc1.h"
#include "asterfort/mltpre.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: nommat, renumz
    integer :: npivot, neq
    real(kind=8) :: eps
    integer :: typsym
!     ------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
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
!     FACTORISATION DE GAUSS PAR LA MULTIFRONTALE
!     D'UNE MATRICE SYMETRIQUE A COEFFICIENTS REELS
!     DEVELOPPEMENT MAJEUR DU 14/02/00
!     I) VERSION MONOPROCESSEUR  OU PARALLELE MLTFC1 OU MLTFCB
!     II)GESTION DE LA MEMOIRE:
!     SI LA PILE TIENT ENTIERE EN MEMOIRE: MLTFC1
!     SINON APPEL A MLTFCB
!     MLTFC1 TRAVAILLE DE 1 A NBLOC, SUIVANT LE MODE
!     HABITUEL (3 BLOCS PERMANENTS EN MEMOIRE)
!     ------------------------------------------------------------------
!
!     IN  NOMMAT  :    : NOM UTILISATEUR DE LA MATRICE A FACTORISER
!
!     VAR PIVOT   : IS :
!     : EN SORTIE : NPIVOT  = 0 ==> R.A.Z.
!     :    NPIVOT  > 0 ==> MATRICE SINGULIERE
!     POUR L'EQUATION DE NUMERO NPIVOT
!     :    NPIVOT  < 0 ==> -NPIVOT TERMES DIAGONAUX < 0
!
!     IN  NEQ     : IS : NOMBRE TOTAL D'EQUATION
!
!     IN  RENUMZ : K* : METHODE DE RENUMEROTATION MD/MDA/METIS
!     :SI RENUMZ=' ' : CELLE DU SOLVEUR PAR DEFAUT DE LA MATRICE
!
!     ------------------------------------------------------------------
    integer :: k, nc, ierr
    character(len=14) :: nu
    character(len=19) :: noma19
    character(len=24) :: nmprvr, nmprvi, nmprv2, nmpri2, nmprcl, nmprcu, nmprt1
    character(len=24) :: nomloc, factol, factou, nomadi, nompil, nmprt2, nomadj
    character(len=24) :: nomdia, nmpilu, nompr1
    character(len=24) :: nomp01, nomp02, nomp03, nomp04, nomp05, nomp06, nomp07
    character(len=24) :: nomp08, nomp09, nomp10, nomp11, nomp12, nomp13, nomp14
    character(len=24) :: nomp15, nomp16, nomp17, nomp18, nomp19, nomp20
    integer :: ldiag, long, ifac, sni, isnd, adfac0, adfac
!     -------------------------------------------------- POINTEURS
    integer :: tempi
    integer :: supnd
    integer :: seq, fils, frere, adress, lfront, nblign, lgsn
    integer :: nbass, decal, local
    integer :: adpile, lgbloc, pile
    integer :: ncbloc, adinit, adjnit
!     -------------------------------------------------- VARIABLES
    integer :: anc, tabi2, tabr2, i, j, trav1, trav2
    integer :: lonmat, nbsn
    integer :: lgpile, nbloc, mxmate, ln, adbl1
    integer :: ib, desc, it(5), mxbloc, ltempr, nb
    real(kind=8) :: temps(6)
    integer :: nproc, ifm, niv, lpmax, iadigs
!     NB : ORDRE DES MATRICES CL ET CU (LES PRODUITS MATRICE*MATRICE)
!     96 EST OPTIMUM POUR EV68, 32 EST OPTIMUM POUR PENTIUM 4
    integer :: cl, cu, jrefa
!     ------------------------------------------------------------------
    data nompr1/'&&MLFC16.PROVISOI.REELS1'/
    data nmprvi/'&&MLFC16.PROVISOI_ENTIE '/
    data nmprv2/'&&MLFC16.PROVISOI.REELS '/
    data nmpri2/'&&MLFC16.PROVISOI.ENTIE '/
    data nmprvr/'&&MLFC16.PROVISOI_REELS '/
    data nmprcl/'&&MLFC16.PROVISOI_REELS3'/
    data nmprcu/'&&MLFC16.PROVISOI_REELS4'/
    data nmprt1/'&&MLFC16.PROVISOI_REELS5'/
    data nmprt2/'&&MLFC16.PROVISOI_REELS6'/
!
    data factol/'                   .VALF'/
    data factou/'                   .WALF'/
!
    data nompil/'&&MLFC16.PILE_MATRICE_FR'/
    data nmpilu/'&&MLFC16.PILE_MATRICU_FR'/
    data nomdia/'                   .&VDI'/
    data nomadj/'&&MLFC16.PROVISOI_ENTIEJ'/
!     ------------------------------------------------------------------
    call jemarq()
!
    call infniv(ifm, niv)
!----------------------------------------------------------------------
    nb=llbloc()
    nb=nb/2
    noma19 = nommat
    npivot = 0
!
!     -- ON FAIT LA FACTORISATION SYMBOLIQUE SI NECESSAIRE :
    call mltpre(noma19, renumz)
!
    call dismoi('NOM_NUME_DDL', noma19, 'MATR_ASSE', repk=nu)
    nomloc = nu//'.MLTF.LOCL'
    nomadi = nu//'.MLTF.ADNT'
    call mlnmin(nu, nomp01, nomp02, nomp03, nomp04,&
                nomp05, nomp06, nomp07, nomp08, nomp09,&
                nomp10, nomp11, nomp12, nomp13, nomp14,&
                nomp15, nomp16, nomp17, nomp18, nomp19,&
                nomp20)
    ierr = 0
    factol(1:19) = nommat
    factou(1:19) = nommat
    call jeveuo(nomadi, 'L', adinit)
!
    call jeveuo(nomp01, 'L', desc)
    call jeveuo(nomp16, 'L', lgbloc)
    call jeveuo(nomp08, 'L', lgsn)
!
    neq = zi(desc)
    nomdia(1:19) = nommat
    nbsn = zi(desc+1)
    nbloc = zi(desc+2)
    lgpile = zi(desc+3)
    if (typsym .eq. 0) lgpile = 2*lgpile
    lonmat = zi(desc+4)
    call jelibe(nomp01)
    call wkvect(nomadj, ' V V I ', lonmat, adjnit)
    do 1 i = 0, lonmat-1
        zi(adjnit+i)=zi(adinit+i)
  1 end do
    call jelibe(nomadi)
!
!
    call mltasc(nbloc, zi(lgbloc), zi(adjnit), nommat, lonmat,&
                factol, factou, typsym)
    call jedetr(nomadj)
!
!     RECUPERATION DU NOMBRE DE PROCESSEURS
    nproc = mlnbpr()
    call jedisp(2, it)
!
    mxbloc = 0
    do 20 i = 1, nbloc
        mxbloc = max(mxbloc,zi(lgbloc+i-1))
 20 end do
    lpmax = zi(lgsn)
    mxmate= lpmax*(lpmax+1)/2
    do 10 i = 1, nbsn-1
        ln = zi(lgsn+i)
        mxmate = max(mxmate,ln*(ln+1)/2)
        lpmax = max(lpmax,ln)
 10 end do
    if (niv .ge. 2) then
        write (ifm,*) ' AVANT FACTORISATION '//'LONGUEURS DISPONIBLES ',&
     &        it(1),'ET ',it(2),'LONGUEUR DE LA PILE ',lgpile,&
     &        ', PLUS GRAND BLOC DE FACTOL ',mxbloc
        write (ifm,*) 'PLUS GRAND BLOC DE MATRICES FRONTALES: ',&
        mxmate
        write (ifm,*) ' NOMBRE DE PROCESSEURS : ',nproc
        write (ifm,*) ' TYPSYM : ',typsym
    endif
!
! ######################################################################
!
!     ON ALLOUE LA PILE
    call wkvect(nompil, ' V V C ', lgpile, pile)
    if (niv .eq. 2) write (ifm,*) ' => PILE TOUT EN MEMOIRE '
    call wkvect(nompr1, ' V V C ', mxbloc, adbl1)
!
!
!
!--------------------------------------------------------------------
    call jeveuo(nomloc, 'L', local)
    call jeveuo(nomp03, 'L', adress)
    call jeveuo(nomp04, 'L', supnd)
    call jeveuo(nomp06, 'L', fils)
    call jeveuo(nomp07, 'L', frere)
    call jeveuo(nomp08, 'L', lgsn)
    call jeveuo(nomp09, 'L', lfront)
    call jeveuo(nomp10, 'L', nbass)
    call jeveuo(nomp13, 'L', adpile)
    call jeveuo(nomp14, 'L', anc)
    call jeveuo(nomp15, 'L', nblign)
    call jeveuo(nomp16, 'L', lgbloc)
    call jeveuo(nomp17, 'L', ncbloc)
    call jeveuo(nomp18, 'L', decal)
    call jeveuo(nomp20, 'L', seq)
    ltempr=nb*lpmax*nproc
    call wkvect(nmprt1, ' V V C ', ltempr, trav1)
    call wkvect(nmprt2, ' V V C ', ltempr, trav2)
    call wkvect(nmprcl, ' V V C ', nproc*nb**2, cl)
    call wkvect(nmprcu, ' V V C ', nproc*nb**2, cu)
    call wkvect(nmprv2, ' V V C ', neq, tabr2)
    call wkvect(nmprvi, ' V V I ', neq, tempi)
    call wkvect(nmpri2, ' V V I ', neq, tabi2)
    call uttcpu('CPU.MLFC16', 'INIT', ' ')
    call uttcpu('CPU.MLFC16', 'DEBUT', ' ')
!     3.2)                               ASSEMBLAGE ET FACTORISATION
!     APPEL A MLTFAS1
    call jedetr(nompr1)
    call mltcc1(nbloc, zi(ncbloc), zi(decal), zi(supnd), zi(fils),&
                zi(frere), zi(seq), zi(lgsn), zi(lfront), zi(adress),&
                zi4(local), zi(adpile), zi(nbass), zc(pile), lgpile,&
                zi(tempi), zc(trav1), zc(trav2), factol, factou,&
                typsym, zi(tabi2), eps, ierr, nb,&
                zc(cl), zc(cu))
    if (ierr .gt. 0) goto 9998
!
    call jelibe(nomloc)
!     RECUPERATION DE LA DIAGONALE 'APRES':
!     VERSION MODIFIEE POUR L' APPEL A DGEMV (PRODUITS MATRICE-VECTEUR)
!     LE STOCKAGE DES COLONNES DE LA FACTORISEE EST MODIFIE
!
!     --- CREATION D'UN TABLEAU POUR STOCKER LA DIAGONALE
    call wkvect(nomdia, 'V V C', neq, ldiag)
    isnd = 0
    do 50 ib = 1, nbloc
        call jeveuo(jexnum(factol, ib), 'L', ifac)
        adfac0 = ifac - 1
!
        do 40 nc = 1, zi(ncbloc+ib-1)
            isnd = isnd + 1
            sni = zi(seq+isnd-1)
            long =zi(adress+sni) - zi(adress+sni-1)
            do 30 k = 1, zi(lgsn+sni-1)
                adfac = adfac0 + (k-1)*long + k
                zc(ldiag-1+zi(supnd-1+sni)+k-1) = zc(adfac)
 30         continue
            adfac0 = adfac0 + long*zi(lgsn+sni-1)
 40     continue
        call jelibe(jexnum(factol, ib))
 50 end do
!     PIVOTS NEGATIFS :
    do 60 i = 1, neq
        if (abs(zc(ldiag+i-1)) .lt. 0.d0) npivot = npivot - 1
 60 end do
    call jeveuo(noma19//'.DIGS', 'E', iadigs)
    do 70 i = 1, neq
        j = zi(anc-1+i)
        zc(iadigs-1+neq+j) = zc(ldiag+i-1)
 70 end do
!
!
!
!     MATRICE SINGULIERE :
9998 continue
    if (ierr .ne. 0) then
        npivot = zi(anc-1+ierr)
    endif
!
    call jeveuo(noma19//'.REFA', 'E', jrefa)
    zk24(jrefa-1+8)='DECT'
!
!
    call uttcpu('CPU.MLFC16', 'FIN', ' ')
    call uttcpr('CPU.MLFC16', 6, temps)
    if (niv .eq. 2) then
        write (ifm,*) ' FACTORISATION DE LA MATRICE.'//'TEMPS CPU',&
        temps(3),' + TEMPS CPU SYSTEME ',temps(6)
    endif
    call jedetr(nomdia)
    call jedetr(nmprt1)
    call jedetr(nmprt2)
    call jedetr(nmprcl)
    call jedetr(nmprcu)
    call jedetr(nmpri2)
    call jedetr(nmprv2)
    call jedetr(nmprvr)
    call jedetr(nmprvi)
    call jedetr(nompil)
    call jedetr(nmpilu)
    call jelibe(nomp01)
    call jelibe(nomp03)
    call jelibe(nomp04)
    call jelibe(nomp06)
    call jelibe(nomp07)
    call jelibe(nomp08)
    call jelibe(nomp09)
    call jelibe(nomp10)
    call jelibe(nomp13)
    call jelibe(nomp14)
    call jelibe(nomp15)
    call jelibe(nomp16)
    call jelibe(nomp17)
    call jelibe(nomp18)
    call jelibe(nomp19)
    call jelibe(nomp20)
    call jedema()
end subroutine
