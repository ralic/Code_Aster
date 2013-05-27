subroutine diag99(nomres)
    implicit  none
    include 'jeveux.h'
    include 'asterc/getvid.h'
    include 'asterfort/copisd.h'
    include 'asterfort/copmod.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mrmult.h'
    include 'asterfort/mtdscr.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rscrsd.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsnoch.h'
    include 'asterfort/vecini.h'
    include 'asterfort/vpgskp.h'
    include 'asterfort/vtcrem.h'
    include 'asterfort/wkvect.h'
    include 'blas/ddot.h'
    character(len=8) :: nomres
!----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!----------------------------------------------------------------------
!
!         DEFI_BASE_MODALE : DIAG_MASS
!
! CE MOT CLE PERMET DE DIAGONALISER LA MATRICE DE MASSE EN DEUX ETAPES
!
! 1- RETIRER AUX MODES STATIQUES LEUR CONTRIBUTION SUR LES
!    MODES DYNAMIQUES
!
! 2- ORTHOGONALISER LA FAMILLES DES MODES STATIQUES MODIFIES PAR
!    LE PROCEDE DE GRAAM-SCHMIDT
!
!----------------------------------------------------------------------
!
!
    integer :: iad, jiad, llref, ier, ibid, jordm, idmode, lmasse, idstat, jords
    integer :: jtrav1, jtrav2, jtrav3, jtrav4, jnsta, i, j, k, ieq, nbord
    integer :: nbmode, nbstat, neq, n1, iorne, iorol, jvale
    real(kind=8) :: alpha, r8scal
    complex(kind=8) :: cbid
    character(len=8) :: k8b, meca, stat
    character(len=14) :: nu
    character(len=24) :: masse, numddl, mailla
    character(len=19) :: chamol, chamne
    integer :: iarg
!----------------------------------------------------------------------
    call jemarq()
!
!----------------------------------------------------------------------
! --- RECUPERATION DES MODES PROPRES
!-----------------------------------------------------------------------
!
    call getvid('DIAG_MASS', 'MODE_MECA', 1, iarg, 1,&
                meca, n1)
!
    call jelira(meca//'           .ORDR', 'LONUTI', nbmode, k8b)
    call jeveuo(meca//'           .ORDR', 'L', jordm)
    call jeveuo(nomres//'           .REFD', 'L', llref)
    masse = zk24(llref+1)
    numddl = zk24(llref+3)
    nu = numddl(1:14)
!
    call dismoi('F', 'NOM_MAILLA', numddl, 'NUME_DDL', ibid,&
                mailla, ier)
    call dismoi('F', 'NB_EQUA', masse, 'MATR_ASSE', neq,&
                k8b, ier)
    call wkvect('&&DIAG99.MODE_MECA', 'V V R', nbmode*neq, idmode)
    call copmod(meca, 'DEPL', neq, nu, nbmode,&
                'R', zr(idmode), cbid)
!
!-----------------------------------------------------------------------
! --- RECUPERATION DES MODES STATIQUES
!-----------------------------------------------------------------------
!
    call getvid('DIAG_MASS', 'MODE_STAT', 1, iarg, 1,&
                stat, n1)
!
    call jelira(stat//'           .ORDR', 'LONUTI', nbstat, k8b)
    call jeveuo(stat//'           .ORDR', 'L', jords)
    call wkvect('&&DIAG99.MODE_STAT', 'V V R', nbstat*neq, idstat)
    call copmod(stat, 'DEPL', neq, nu, nbstat,&
                'R', zr(idstat), cbid)
!
!-----------------------------------------------------------------------
! --- RECUPERATION DU DESCRIPTEUR DE LA MATRICE DE MASSE
!-----------------------------------------------------------------------
    call mtdscr(masse)
    call jeveuo(masse(1:19)//'.&INT', 'L', lmasse)
!
!-----------------------------------------------------------------------
! 1- RETIRER AUX MODES STATIQUES LEUR CONTRIBUTION AUX MODES PROPRES
! MODE STAT J =
! MODE STAT J - SOMME (T(MODE STAT J)*MASSE*MODE PROPRE I)*MODE PROPRE I
! OU T(VECTEUR) EST LA TRANSPOSEE DU VECTEUR
!-----------------------------------------------------------------------
    call wkvect('&&DIAG99.NEW_STAT', 'V V R', nbstat*neq, jnsta)
    call wkvect('&&DIAG99.TRAV1', 'V V R', neq, jtrav1)
    call wkvect('&&DIAG99.TRAV2', 'V V R', neq, jtrav2)
    call wkvect('&&DIAG99.TRAV3', 'V V R', nbstat, jtrav3)
    call wkvect('&&DIAG99.TRAV4', 'V V I', neq, jtrav4)
!
    do 10 j = 1, nbstat
!
        call vecini(neq, 0.d0, zr(jtrav1))
!
        do 20 i = 1, nbmode
!
! --------- PRODUIT MASSE*MODE PROPRE I
            call mrmult('ZERO', lmasse, zr(idmode+(i-1)*neq), zr( jtrav2), 1,&
                        .true.)
!
! --------- (T(MODE STAT J)*MASSE*MODE PROPRE I)
            r8scal=ddot(neq,zr(idstat+(j-1)*neq),1,zr(jtrav2),1)
!
! --------- PRODUIT (T(MODE STAT J)*MASSE*MODE PROPRE I)*MODE PROPRE I
! --------- PUIS
! --------- SOMME (T(MODE STAT J)*MASSE*MODE PROPRE I)*MODE PROPRE I
            do 30 k = 1, neq
                zr(jtrav1+(k-1)) = zr( jtrav1+(k-1)) + r8scal * zr( idmode+(i-1)*neq+(k-1) )
30          continue
20      continue
!
        do 40 k = 1, neq
            zr(jnsta+(j-1)*neq+(k-1)) = zr( idstat+(j-1)*neq+(k-1)) - zr(jtrav1+(k-1) )
40      continue
!
10  end do
!
    do 50 i = 1, neq
        zi(jtrav4+i-1) = 1
50  end do
    alpha = 0.717d0
!
    call vpgskp(neq, nbstat, zr(jnsta), alpha, lmasse,&
                2, zr(jtrav1), zi(jtrav4), zr(jtrav3))
!
    nbord = nbmode + nbstat
    call rscrsd('G', nomres, 'MODE_MECA', nbord)
!
    iorne =0
    do 80 i = 1, nbmode
        iorol = zi(jordm+i-1)
        iorne = iorne+1
!
        call rsexch('F', meca, 'DEPL', iorol, chamol,&
                    ier)
        call rsexch(' ', nomres, 'DEPL', iorne, chamne,&
                    ier)
        call copisd('CHAMP', 'G', chamol, chamne)
        call rsnoch(nomres, 'DEPL', iorne)
!
        call rsadpa(meca, 'L', 1, 'NUME_MODE', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'NUME_MODE', iorne,&
                    0, jiad, k8b)
        zi(jiad) = zi(iad)
!
        call rsadpa(meca, 'L', 1, 'FREQ', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'FREQ', iorne,&
                    0, jiad, k8b)
        zr(jiad) = zr(iad)
!
        call rsadpa(meca, 'L', 1, 'NORME', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'NORME', iorne,&
                    0, jiad, k8b)
        zk24(jiad) = zk24(iad)
!
        call rsadpa(meca, 'L', 1, 'OMEGA2', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'OMEGA2', iorne,&
                    0, jiad, k8b)
        zr(jiad) = zr(iad)
!
        call rsadpa(meca, 'L', 1, 'MASS_GENE', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'MASS_GENE', iorne,&
                    0, jiad, k8b)
        zr(jiad) = zr(iad)
!
        call rsadpa(meca, 'L', 1, 'RIGI_GENE', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'RIGI_GENE', iorne,&
                    0, jiad, k8b)
        zr(jiad) = zr(iad)
!
        call rsadpa(meca, 'L', 1, 'TYPE_MODE', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'TYPE_MODE', iorne,&
                    0, jiad, k8b)
        zk16(jiad) = zk16(iad)
!
80  end do
!
    do 90 i = 1, nbstat
        iorol = zi(jords+i-1)
        iorne = iorne+1
!
        call rsadpa(nomres, 'E', 1, 'NUME_MODE', iorne,&
                    0, jiad, k8b)
        zi(jiad) = iorol+nbmode
!
        call rsadpa(nomres, 'E', 1, 'FREQ', iorne,&
                    0, jiad, k8b)
        zr(jiad) = 0.d0
!
        call rsadpa(nomres, 'E', 1, 'OMEGA2', iorne,&
                    0, jiad, k8b)
        zr(jiad) = 0.d0
!
        call rsadpa(nomres, 'E', 1, 'MASS_GENE', iorne,&
                    0, jiad, k8b)
        zr(jiad) = 0.d0
!
        call rsadpa(nomres, 'E', 1, 'RIGI_GENE', iorne,&
                    0, jiad, k8b)
        zr(jiad) = 0.d0
!
        call rsadpa(stat, 'L', 1, 'NOEUD_CMP', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'NOEUD_CMP', iorne,&
                    0, jiad, k8b)
        zk16(jiad) = zk16(iad)
!
        call rsadpa(stat, 'L', 1, 'TYPE_DEFO', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'TYPE_DEFO', iorne,&
                    0, jiad, k8b)
        zk16(jiad) = zk16(iad)
!
        call rsadpa(stat, 'L', 1, 'TYPE_MODE', iorol,&
                    0, iad, k8b)
        call rsadpa(nomres, 'E', 1, 'TYPE_MODE', iorne,&
                    0, jiad, k8b)
        zk16(jiad) = zk16(iad)
!
        call rsexch(' ', nomres, 'DEPL', iorne, chamol,&
                    ier)
        call vtcrem(chamol, masse, 'G', 'R')
        call jeveuo(chamol//'.VALE', 'E', jvale)
        do 111 ieq = 1, neq
            zr(jvale+ieq-1) = zr(jnsta+(i-1)*neq+ieq-1)
111      continue
        call rsnoch(nomres, 'DEPL', iorne)
90  continue
!
    call jedetr('&&DIAG99.TRAV1')
    call jedetr('&&DIAG99.TRAV2')
    call jedetr('&&DIAG99.TRAV3')
    call jedetr('&&DIAG99.TRAV4')
    call jedetr('&&DIAG99.TRAV5')
    call jedetr('&&DIAG99.TRAV6')
    call jedetr('&&DIAG99.MODE_MECA')
    call jedetr('&&DIAG99.MODE_STAT')
!
    call jedema()
end subroutine
