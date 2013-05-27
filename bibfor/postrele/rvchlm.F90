subroutine rvchlm(ssch19, m2d, noeud, nbn, nbcmp,&
                  nbco, nbsp, val)
    implicit none
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    character(len=19) :: ssch19
    integer :: m2d, noeud(*), nbn, nbcmp, nbco, nbsp
    real(kind=8) :: val(*)
!
!**********************************************************************
!
!  OPERATION REALISEE
!  ------------------
!     OPERATION D' EXTRACTION SUR DES NOEUD D' UNE MAILLE
!
!  ARGUMENT EN ENTREE
!  ------------------
!
!     SSCH19 : NOM DU SOUS CHAMP DE GRANDEUR
!     M2D    : NUMERO DE LA MAILLE (2D)
!     NOEUD  : TABLE DES NUMEROS DE NOEUDS
!     NBN    : NOMBRE DE NOEUD A TRAITER
!     NBCMP  : NOMBRE DE CMP A EXTRAIRE
!     NBCO   : NOMBRE DE COUCHES
!     NBSP   : NOMBRE DE COUCHES
!
!  ARGUMENT EN SORTIE
!  ------------------
!
!     VAL : TABLE DES VALEURS EXTRAITES
!
!**********************************************************************
!
!  FONCTIONS EXTERNES
!  ------------------
!
!
!  -----------------------------------------
!
!
!  ---------------------------------
!
!  VARIABLES LOCALES
!  -----------------
!
    character(len=24) :: nvale, npadr, nnumlo, nnoma
    character(len=8) :: mailla
!
    integer :: avale, apadr, anumlo, apnco, apnsp, nbtnd
    integer :: adr, aconec, i, nd, nloc, j, adrm, adrn, lcoi, lndi, lndo, lcoo
!
    logical :: trouve
    character(len=1) :: k1bid
!
!==================== CORPS DE LA ROUTINE =============================
!
!-----------------------------------------------------------------------
    integer :: k
!-----------------------------------------------------------------------
    call jemarq()
    nvale = ssch19//'.VALE'
    npadr = ssch19//'.PADR'
    nnoma = ssch19//'.NOMA'
    nnumlo = '&&RVCHLM.NUM.LOC.NOEUD '
!
    call jeveuo(nnoma, 'L', adr)
    call jeveuo(nvale, 'L', avale)
    call jeveuo(npadr, 'L', apadr)
    call jeveuo(ssch19//'.PNCO', 'L', apnco)
    call jeveuo(ssch19//'.PNSP', 'L', apnsp)
!
    adrm = zi(apadr + m2d-1)
!
    mailla = zk8(adr)
!
    call jeveuo(jexnum(mailla//'.CONNEX', m2d), 'L', aconec)
    call jelira(jexnum(mailla//'.CONNEX', m2d), 'LONMAX', nbtnd, k1bid)
!
    lndi = nbcmp*zi(apnsp + m2d-1)
    lcoi = lndi*nbtnd
    lndo = nbcmp*nbsp
    lcoo = lndo*nbn
!
    call wkvect(nnumlo, 'V V I', nbn, anumlo)
!
    do 100, i = 1, nbn, 1
!
    trouve = .false.
    nd = noeud(i)
    nloc = 1
!
110  continue
    if (.not. trouve) then
!
        if (zi(aconec + nloc-1) .eq. nd) then
!
            trouve = .true.
!
        else
!
            nloc = nloc +1
!
        endif
!
        goto 110
!
    endif
!
    zi(anumlo + i-1) = nloc
!
    100 end do
!
    do 200, i = 1, nbn, 1
!
    adrn = adrm + (zi(anumlo + i-1)-1)*lndi
!
    do 210, j = 1, nbco, 1
!
    do 211, k = 1, lndo, 1
!
    val(k + (j-1)*lcoo + lndo*(i-1)) = zr(avale + adrn-1 + (j-1)*lcoi + k-1)
!
211  continue
!
210  continue
!
    200 end do
!
    call jedetr(nnumlo)
!
    call jedema()
end subroutine
