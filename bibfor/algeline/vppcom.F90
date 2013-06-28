subroutine vppcom(lcomod, icom1, icom2, resui, resur,&
                  resuk, nbpari, nbparr, nbpark, mxresf,&
                  vectr, nconv, neq, typres)
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/comatr.h'
    include 'asterfort/comcou.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/maxint.h'
    include 'asterfort/mpicm1.h'
    include 'asterfort/mpiexe.h'
    include 'asterfort/somint.h'
    include 'asterfort/vecink.h'
    include 'asterfort/vecint.h'
    include 'asterfort/wkvect.h'
    include 'blas/dcopy.h'
    logical :: lcomod
    integer :: icom1, icom2, nbpari, nbparr, nbpark, nconv, neq, mxresf
    integer :: resui(*)
    real(kind=8) :: vectr(*), resur(*)
    character(len=16) :: typres
    character(len=*) :: resuk(*)
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
!     ----------------------------------------------------------------
!     COMMUNICATION DES VECTEURS PROPRES ET SD ASSOCIEES POUR MACRO
!     MODE_MECA //
!     IN  LCOMOD      :  LOG  : MACRO_MODE_MECA// OR NOT
!     IN  ICOM1/ICOM2 :  IN   : PARAMETRES // ASSOCIES A LCOMOD=.TRUE.
!     IN/OUT RESUI/RESUR/RESUK: VECTEURS I/R/K : VECTEURS ASSOCIES A LA
!                               SD_MODE_MECA A PRODUIRE.
!     IN NBPARI/NBPARR/NBPARK : IN : NBRE DE COLONNES DES CES SDS.
!     IN MXRESF       :  IN   : NBRE DE LIGNES UTILISEES POUR CES SDS.
!     IN/OUT VECTR    : R     : VECTEURS PROPRES REELS
!     IN/OUT NCONV    :  IN   : NBRE DE MODES PROPRES CONVERGES DU PROC
!                (EN IN), NBRE TOTAL DE MODES CV (EN OUT).
!     IN NEQ          :  IN   : TAILLE DU PROBLEME
!     IN TYPRES       :  IN   : TYPE DE RESULTATS (DYNAMIQUE OU FLAMB)
!     ----------------------------------------------------------------
    include 'jeveux.h'
!
!     --- VARIABLES LOCALES
    integer :: nconvl, nconvg, nconvm, rangl, rangll, mpicow, mpicou, mpico0
    integer :: ibid, l1, l2, izero, i, idecal, j, i8, jlcom, jlbuff, jlbufs, ifm
    integer :: niv, ietfin, ietdeb, ietrat, ietmax
    integer(kind=4) :: i4
    real(kind=8) :: retfin, rbid
    complex(kind=8) :: cbid
    character(len=1) :: k1bid
    character(len=24) :: klcom, k24buf, k24bus, k24b
!      LOGICAL      LCPU
!
    call jemarq()
    call infniv(ifm, niv)
!     --- POUR MESURER LE TEMPS CONSOMMEE DS LES ETAPES DE OP0045
!     --- IL FAUT AUSSI DECOMMENTER LES APPELS A SYSTEM_CLOCK (UTILES
!     --- MAIS REFUSES PAR L'AGLA).
!      LCPU=.TRUE.
!      LCPU=.FALSE.
    izero=0
    k24buf='&&OP0045.BUFFMPI'
    k24bus='&&OP0045.BUFFMPI.SAVE'
    klcom='&&OP0045.LCOMOD1'
    if (lcomod) then
        mpicow=comcou(0)
        mpicou=comcou(1)
!       --- ON EST CENSE FONCTIONNER JUSQUE LA EN COM LOCAL
!       --- (POUR SOLVEUR LINEAIRE DU SOLVEUR MODAL)
        if (mpicow .eq. mpicou) call assert(.false.)
        call mpiexe('MPI_RANG_SIZE', mpicou, ibid, rangl, ibid)
!       ----------------------------------------------------------------
!       --- STEP 0: COMM AU SEIN DU COM_WORLD
!       ----------------------------------------------------------------
!       --- ON REMET LE MPICOW POUR LES COMS DE NCONVG ET DE KLCOM.
!       --- ON GARDE LE COM LOCAL MPICOU POUR LE BCAST ULTERIEUR
!       --- ON PREPARE UN COM LOCAL RESERVE A TOUS LES PROCS DE
!       --- RANGL=0: MPICO0
!       --- DANS CE NEW COM MPICO0, LE RANG EST NOTE RANGLL. IL DOIT
!       --- ETRE IDENTIQUE A ICOM1-1.
        call mpiexe('AFFE_COMM_REFE', mpicow, ibid, 1, ibid)
        call mpicm1('BARRIER', k1bid, ibid, ibid, ibid,&
                    rbid, cbid)
!        IF (LCPU) THEN
!          CALL MPICM1('BARRIER',K1BID,IBID,IBID,IBID,RBID,CBID)
!          CALL SYSTEM_CLOCK(IETDEB,IETRAT,IETMAX)
!        ENDIF
        if (rangl .eq. 0) then
            l1=1
        else
            l1=2
        endif
        call mpiexe('MPI_COMM_SPLIT', mpicow, mpico0, l1, icom1)
        if (mpicow .eq. mpico0) call assert(.false.)
        call mpiexe('MPI_RANG_SIZE', mpico0, ibid, rangll, l2)
        if ((l2.ne.icom2) .and. (rangl.eq.0)) call assert(.false.)
        if ((rangll.ne.(icom1-1)) .and. (rangl.eq.0)) call assert(.false.)
!
!       --- VECTEUR DES DECALAGES (NCONV) PAR PROC POUR VECTEURS PROPRES
!       --- CALCUL ET AFFECTATION DU NBRE TOTAL DE MODES CONVERGES
        call wkvect(klcom, 'V V I', icom2, jlcom)
        call vecint(icom2, izero, zi(jlcom))
!       --- NCONVL: NBRE DE MODES CONVERGES DU CALCUL ASSOCIE AU PROC
!       --- ZI(JLCOM+ICOM1-1)=NCONVL SI RANGL=0, 0 SINON
!       --- NCONV=NCONVG: NBRE DE MODES CONVERGES TOTAL
!       --- NCONVM=MAX(NCONVL) (POUR DIMENSIONNER BUFFER DE COM)
        nconvl=nconv
        if (rangl .eq. 0) zi(jlcom+icom1-1)=nconvl
        call mpicm1('MPI_SUM', 'I', icom2, ibid, zi(jlcom),&
                    rbid, cbid)
        nconvg=somint(icom2,zi(jlcom))
        if (nconvg .le. 0) call assert(.false.)
        nconv=nconvg
        nconvm=maxint(icom2,zi(jlcom))
        if (nconvm .le. 0) call assert(.false.)
!
!       ----------------------------------------------------------------
!       --- STEP 1: COMM ENTRE LES MAITRES DE CHAQUE SOUS-BANDES
!       ----------------------------------------------------------------
!       --- ON AFFECTE LE COMCO0 POUR COMMUNIQUER ENTRE SOUS-BANDES
!       --- LES VECTEURS PROPRES
!       --- PUIS COMM DES &&OP0045.RESU_R
!       --- COMM DES &&OP0045.RESU_I ET K
        call mpicm1('BARRIER', k1bid, ibid, ibid, ibid,&
                    rbid, cbid)
        call mpiexe('AFFE_COMM_REFE', mpico0, ibid, 1, ibid)
        if (rangl .eq. 0) then
!         --- 2 BUFFERS: K24BUF POUR LE BCAST, LE K24BUS POUR SAUVE
!         --- GARDER LES VECTEURS PROPRES EN ATTENDANT LEUR COMM AUX
!         --- AUTRES PROCESSUS DE MPICO0.
            call wkvect(k24buf, 'V V R', nconvm*neq, jlbuff)
            call wkvect(k24bus, 'V V R', nconvm*neq, jlbufs)
            i4=nconvl*neq
            call dcopy(i4, vectr, 1, zr(jlbufs), 1)
            do 115 i = 1, icom2
                idecal=0
                do 114 j = 1, i-1
                    idecal=idecal+zi(jlcom+j-1)
114              continue
                if (idecal .lt. 0) call assert(.false.)
                i8=neq*zi(jlcom+i-1)
                i4=i8
                if (i .eq. icom1) call dcopy(i4, zr(jlbufs), 1, zr(jlbuff), 1)
                call mpicm1('BCASTP', 'R', i8, i-1, ibid,&
                            zr(jlbuff), cbid)
                call dcopy(i4, zr(jlbuff), 1, vectr(1+idecal*neq), 1)
115          continue
            call jedetr(k24bus)
            call jedetr(k24buf)
!
            call comatr('T', 'R', icom2, icom1-1, zi(jlcom),&
                        0, 0, [ibid], mxresf, nbparr,&
                        resur, 0, 0, [cbid])
            call comatr('T', 'I', icom2, icom1-1, zi(jlcom),&
                        mxresf, nbpari, resui, 0, 0,&
                        [rbid], 0, 0, [cbid])
        endif
!
!       ----------------------------------------------------------------
!       --- STEP 2: BARRIERE SUR LE COM_WORLD AU CAS OU
!       ----------------------------------------------------------------
!       -- ON AFFECTE LE COMCOW POUR QUE TOUS LES PROCS S'ATTENDENT
        call mpiexe('AFFE_COMM_REFE', mpicow, ibid, 1, ibid)
        call mpicm1('BARRIER', k1bid, ibid, ibid, ibid,&
                    rbid, cbid)
!        IF (LCPU) THEN
!          CALL SYSTEM_CLOCK(IETFIN)
!          RETFIN=REAL(IETFIN-IETDEB)/REAL(IETRAT)
!          WRITE(IFM,*)'<VPPCOM> COUT COM 1  VP + ZR/ZI: ',RETFIN
!          CALL SYSTEM_CLOCK(IETDEB,IETRAT,IETMAX)
!        ENDIF
!
!       ----------------------------------------------------------------
!       --- STEP 3: COM AU SEIN DE CHAQUE SOUS-BANDES
!       ----------------------------------------------------------------
!       --- ON AFFECTE LE COMCOU POUR COMMUNIQUER AU SEIN
!       --- D'UNE SOUS-BANDE.
        call mpiexe('AFFE_COMM_REFE', mpicou, ibid, 1, ibid)
!
!       --- POUR GAGNER DU TEMPS, ON ENVOIE VP PAR VP
!       --- ON COMMUNIQUE LES VECTEURS PROPRES EN SOUS-PAQUETS DE REELS
!       --- DE TAILLE =< SIZBMPI POUR EVITER LES PBS DE CONTENTIONS
!       --- MEMOIRE ET LES LIMITES DES ENTIERS COURTS MPI.
        do 116 i = 1, nconvg
            call mpicm1('BCASTP', 'R', neq, 0, ibid,&
                        vectr(1+(i-1)*neq), cbid)
116      continue
!       --- ON COMMUNIQUE LES PETITS OBJETS SUIVANTS
        call mpicm1('BCAST', 'R', nbparr*mxresf, 0, ibid,&
                    resur, cbid)
        call mpicm1('BCAST', 'I', nbpari*mxresf, 0, resui,&
                    rbid, cbid)
!
!       ----------------------------------------------------------------
!       --- STEP 4: BARRIERE SUR LE COM_WORLD AU CAS OU
!       ----------------------------------------------------------------
!       --- ON AFFECTE LE COMCOW POUR QUE TOUS LES PROCS S'ATTENDENT
        call mpiexe('AFFE_COMM_REFE', mpicow, ibid, 1, ibid)
        call mpicm1('BARRIER', k1bid, ibid, ibid, ibid,&
                    rbid, cbid)
!        IF (LCPU) THEN
!          CALL SYSTEM_CLOCK(IETFIN)
!          RETFIN=REAL(IETFIN-IETDEB)/REAL(IETRAT)
!          WRITE(IFM,*)'<VPPCOM> COUT COM 2  VP + ZR/ZI: ',RETFIN
!        ENDIF
!
!       --- ON REMET LE COM LOCAL AU CAS OU
        call mpiexe('AFFE_COMM_REFE', mpicou, ibid, 1, ibid)
!
!       ----------------------------------------------------------------
!       --- STEP 5: INIT DIVERSES.
!       ----------------------------------------------------------------
        if (typres(1:9) .ne. 'DYNAMIQUE') then
            do 121 i = 1, nconvg
                resui(i)=i
121          continue
        endif
        do 125 i = 1, nbpark
            j=1+(i-1)*mxresf
            k24b=resuk(j)
            if (k24b .ne. resuk(j+1)) call assert(.false.)
            call vecink(nconvg, k24b, resuk(j))
125      continue
!       ----------------------------------------------------------------
!       --- STEP 6: MENAGE.
!       ----------------------------------------------------------------
        call jedetr(klcom)
        call mpiexe('MPI_COMM_FREE', mpico0, ibid, ibid, ibid)
    endif
!
    call jedema()
!
end subroutine
