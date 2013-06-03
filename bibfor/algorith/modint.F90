subroutine modint(ssami, raiint, nddlin, nbmod, shift,&
                  matmod, masse, raide, neq, coint,&
                  noddli, nnoint, vefreq, switch)
! aslint: disable=W1501
    implicit none
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
!-----------------------------------------------------------------------
!    M. CORUS     DATE 05/02/10
!-----------------------------------------------------------------------
!
!  BUT:      < CALCUL DES MODES D'INTERFACE >
!
!  ON EXTRAIT, A PARTIR DE LA SOUS MATRICE ASSOCIEE A L'INTERFACE, LA
!  CONNECTIVITE DU TREILLIS DE POUTRE SOUS JACENT. ON DETERMINE LE
!  NOMBRE DE PARTIES INDEPENDANTES DE L'INTERFACE, ET ON CALCULE, POUR
!  CHAQUE PARTIE, LES PREMIERS MODES PROPRES, EN PRENANT SOIN DE BIEN
!  CAPTER LES MODES DE CORPS RIGIDE. ON CONSTRUIT ENSUITE, SUR LA BASE
!  DE CES MODES, UN SOUS ESPACE POUR PROJETER LES MATRICES DU PROBLEME
!  COMPLET, ET ON CALCULE, SUR CE SOUS ESPACE, LES MODES D'INTERFACE.
!
!-----------------------------------------------------------------------
!  IN  : SSAMI   : MATRICE DE MASSE DU MODELE D'INTERFACE
!  IN  : RAIINT  : MATRICE DE RAIDEUR DU MODELE D'INTERFACE
!  IN  : NDDLIN  : NOMBRE D'EQUATIONS DU NUME_DDL D'INTERFACE
!  IN  : NBMOD   : NOMBRE DE MODES D'INTERFACE DEMANDE
!  IN  : SHIFT   : VALEUR DE FREQUENCE POUR LE DECALAGE DE LA RAIDEUR
!  OUT : MATMOD  : MATRICE DES MODES D'INTERFACE
!  IN  : MASSE   : MATRICE DE MASSE DU MODELE COMPLET
!  IN  : RAIDE   : MATRICE DE RAIDEUR DU MODELE COMPLET
!  IN  : NEQ     : NOMBRE D'EQUATIONS DU NUME_DDL COMPLET
!  IN  : COINT   : DEFINITION DE LA CONNECTIVITE DE L'INTERFACE
!  IN  : NODDLI  : DEFINITION DES DDL PORTES PAR LES NOEUDS D'INTERFACE
!  IN  : NNOINT  : NOMBRE DE NOEUD A L'INTERFACE
!  OUT : VEFREQ  : NOM DU VECTEUR CONTENANT LES FREQUENCES PROPRES
!  IN  : SWITCH  : 1 SI ON DOIT RELEVER LES MODES SUR TOUTE LA SST
!                  0 SI ON NE CONSERVE QUE LES MODES SUR L'INTERFACE
!
!-----------------------------------------------------------------------
!
!
!
!
!     ------------------------------------------------------------------
!
!-- VARIABLES EN ENTREES / SORTIE
    include 'jeveux.h'
    include 'asterc/getran.h'
    include 'asterc/getvr8.h'
    include 'asterc/matfpe.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/intdis.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mrmult.h'
    include 'asterfort/mtcmbl.h'
    include 'asterfort/mtdefs.h'
    include 'asterfort/mtdscr.h'
    include 'asterfort/preres.h'
    include 'asterfort/resoud.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    include 'blas/ddot.h'
    include 'blas/dgeev.h'
    include 'blas/dggev.h'
    integer :: nddlin, nbmod, nnoint, neq, switch
    real(kind=8) :: shift
    character(len=19) :: masse, raide, ssami, raiint
    character(len=24) :: coint, noddli, matmod, vefreq
!
!-- VARIABLES DE LA ROUTINE
    integer :: lmatmo, i1, j1, k1, m1, n1, l1, lmakry, linddl, nsekry, lvtemp
    integer :: lvtmp2, lwr, lwi, linlag, lalpi, lbeta, lmatk, lmatm, lmapro
    integer :: lkpro, lmored, lmatrm, lmatrk, lwork, llwork, lalpr, lddld, lfreq
    integer :: lindfr, lkryl, lmath, limped, lmolol, lmolor, lmatma, iret
    integer :: nbvect, ibid, decal, jwork, ldelg, no, nbsst, lindin, coeff, lvp
    integer :: lintrf, info
    real(kind=8) :: temp, pi, rbid, norm, lambda, comlin(2), swork(1), rand, max
    real(kind=8) :: abs
    parameter    (pi=3.141592653589793238462643D0)
    complex(kind=8) :: cbid
    character(len=1) :: listyp(2)
    character(len=19) :: lismat(2), imped, solveu, nume91, nume, prno
    character(len=24) :: valk
    integer :: iarg
!
!-- DEBUT --C
!
    call jemarq()
!
!------------------------------------------------------------C
!--                                                        --C
!-- CONSTRUCTION DES MATRICES D'IMPEDANCE DYNAMIQUE K+MU*M --C
!--            ET DE MASSE DU MODELE D'INTERFACE           --C
!--                                                        --C
!------------------------------------------------------------C
!
    call mtdscr(ssami)
    call jeveuo(ssami(1:19)//'.&INT', 'L', lmatma)
!
    imped='&&MOIN93.RAID_SHIFT'
    call mtdefs(imped, raiint, 'V', ' ')
    lismat(1)=raiint
    lismat(2)=ssami
    if (switch .eq. 1) then
        call getvr8('MODE_INTERF', 'SHIFT', 1, iarg, 1,&
                    rbid, ibid)
        shift=-(rbid*2.d0*pi)**2
    endif
!
    comlin(1)=1.d0
    comlin(2)=shift
    listyp(1)='R'
    listyp(2)='R'
!
    call mtcmbl(2, listyp, comlin, lismat, imped,&
                ' ', nume91, 'ELIM1')
    call mtdscr(imped)
    call jeveuo(imped(1:19)//'.&INT', 'E', limped)
    call dismoi('F', 'SOLVEUR', ssami, 'MATR_ASSE', ibid,&
                solveu, ibid)
    call dismoi('F', 'NOM_NUME_DDL', ssami, 'MATR_ASSE', ibid,&
                nume91, ibid)
    call preres(solveu, 'V', iret, '&&OP0091.MATPRE', imped,&
                ibid, 1)
!
    if (iret .eq. 2) then
        valk = imped
        call u2mesk('F', 'ALGELINE4_37', 1, valk)
    endif
!
!-------------------------------------------------------------------C
!--                                                               --C
!-- RECUPERATION DU NOMBRE DE STRUCTURES DISJOINTES A L'INTERFACE --C
!--      AINSI QUE LES CONNECTIVITES PAR DECOMPOSITION QR         --C
!--                                                               --C
!-------------------------------------------------------------------C
!
    call intdis(coint, nnoint, noddli, '&&MODINT.INTERFACES_SST ', nbsst)
!
    call jeveuo('&&MODINT.INTERFACES_SST ', 'L', lindin)
!
!------------------------------------------------------C
!--                                                  --C
!-- CALCUL DES MODES DU MODELE D'INTERFACE (ARNOLDI) --C
!--                                                  --C
!------------------------------------------------------C
!
!-- ESTIMATION DU NOMBRE DE MODES A CALCULER PAR SOUS STRUCURE
!
    norm=dble(nbmod/nbsst)
    temp=6.d0/nbsst
!
    coeff=3
    if (norm .gt. 7) then
        nbvect=coeff*(int(norm)+2*(int(temp)+1))
    else
        nbvect=coeff*(6+int(norm)+2)
    endif
    nsekry=int(nbvect*nbsst/coeff)
    coeff=int(nbvect/coeff)
    write(6,*)'------------------------------------------------',&
     &'------------------------'
    write(6,*)' VOUS AVEZ DEMANDE',nbmod,' MODES'
    write(6,*)' LA TAILE DU SOUS ESPACE RETENU EST',nsekry
    write(6,*)'------------------------------------------------',&
     &'------------------------'
!
    call wkvect('&&MODINT.VECT_TEMP', 'V V R', 6*nnoint, lvtemp)
    call wkvect('&&MODINT.VECT_TEMP_2', 'V V R', 6*nnoint, lvtmp2)
    call wkvect('&&MODINT.KRYLOV_INT', 'V V R', 6*nnoint*nbvect, lkryl)
    call wkvect('&&MODINT.HESSENBERG', 'V V R', nbvect**2, lmath)
!
!-- ALLOC. DES MATRICES DE TRAVAIL POUR LE CALCUL DES VALEURS PROPRES
    call wkvect('&&MODINT.LEFT_MODES', 'V V R', nbvect**2, lmolol)
    call wkvect('&&MODINT.RIGHT_MODES', 'V V R', nbvect**2, lmolor)
    no=max(nsekry,nbvect)
    call wkvect('&&MODINT.REAL_PART', 'V V R', no, lwr)
    call wkvect('&&MODINT.IMAG_PART', 'V V R', no, lwi)
    call wkvect('&&MODINT.V_F_PRO', 'V V R', no, lfreq)
    call wkvect('&&MODINT.V_IND_F_PRO', 'V V I', no, lindfr)
!
!-- ALLOCATION DE LA MATRICE CONTENANT LA BASE DU SE DE KRYLOV
    call wkvect('&&MODINT.SE_KRYLOV', 'V V R', neq*nsekry, lmakry)
    call jeveuo('&&MOIN93.IS_DDL_INTERF  ', 'L', lddld)
!
!-- VECTEUR D'INDICES
    call jeveuo('&&MOIN93.V_IND_DDL_INT', 'L', linddl)
    call jeveuo('&&MOIN93.V_IND_LAG', 'L', linlag)
    call jeveuo('&&MOIN93.DDL_ACTIF_INT', 'L', lintrf)
!
!--
!-- BOUCLE SUR LES PARTIES D'INTERFACE DISJOINTES
!--
!
!-- ON DESACTIVE LE TEST FPE
    call matfpe(-1)
!
    do 70 n1 = 1, nbsst
!
!-- TIRAGE ALEATOIRE DU VECTEUR INITIAL
        norm=0.d0
        do 80 i1 = 1, 6*nnoint
            call getran(rand)
            zr(lkryl+i1-1)=(rand*2-1)*zi(lindin+6*nnoint*(n1-1)+i1-1)
            norm=norm+zr(lkryl+i1-1)**2
80      continue
!
        norm=sqrt(norm)
        do 90 i1 = 1, 6*nnoint
            zr(lkryl+i1-1)=zr(lkryl+i1-1)/norm
90      continue
!
!-- REMPLISSAGE DE LA MATRICE DE HESSENBERG ET DU SE DE KRYLOV ASSOCIE
        do 100 k1 = 2, nbvect+1
            do 110 i1 = 1, 6*nnoint
                zr(lvtemp+i1-1)=zr(lkryl+i1-1+(k1-2)*6*nnoint)
110          continue
!
            call mrmult('ZERO', lmatma, zr(lvtemp), zr(lvtmp2), 1,&
                        .true.)
            call resoud(imped, ' ', solveu, ' ', 1,&
                        ' ', ' ', ' ', zr(lvtmp2), cbid,&
                        ' ', .true., 0, iret)
            do 120 j1 = 1, k1-1
                norm=ddot(6*nnoint,zr(lvtmp2),1, zr(lkryl+(j1-1)*6*&
                nnoint),1)
                zr(lmath+(k1-2)*nbvect+j1-1)=norm
                do 130 i1 = 1, 6*nnoint
                    zr(lvtmp2+i1-1)=zr(lvtmp2+i1-1)- norm*zr(lkryl+(&
                    j1-1)*6*nnoint+i1-1)
130              continue
120          continue
!
            norm=ddot(6*nnoint,zr(lvtmp2),1,zr(lvtmp2),1)
            norm=sqrt(norm)
            if (k1 .lt. nbvect+1) then
                zr(lmath+(k1-2)*nbvect+k1-1)=norm
                do 140 i1 = 1, 6*nnoint
                    zr(lkryl+(k1-1)*6*nnoint+i1-1)=zr(lvtmp2+i1-1)/&
                    norm
                    zr(lvtemp+i1-1)=zr(lkryl+(k1-1)*6*nnoint+i1-1)
140              continue
            endif
100      continue
!
!-- RESOLUTION DU PROBLEME AUX VALEURS PROPRES
        call dgeev('N', 'V', nbvect, zr(lmath), nbvect,&
                   zr(lwr), zr(lwi), zr(lmolol), nbvect, zr(lmolor),&
                   nbvect, swork, -1, iret)
        lwork=int(swork(1))
        call wkvect('&&MODINT.MATR_EIGEN_WORK', 'V V R', lwork, jwork)
        call dgeev('N', 'V', nbvect, zr(lmath), nbvect,&
                   zr(lwr), zr(lwi), zr(lmolol), nbvect, zr(lmolor),&
                   nbvect, zr(jwork), lwork, iret)
        call jedetr('&&MODINT.MATR_EIGEN_WORK')
!
!-- TRI DES VALEURS PROPRES
        do 150 i1 = 1, nbvect
            temp=1.d+16
            do 160 j1 = 1, nbvect
                norm=abs(1.d0/sqrt(zr(lwr+j1-1)**2+zr(lwi+j1-1)**2)+&
                shift)
                if (norm .lt. temp) then
                    temp=norm
                    zi(lindfr+i1-1)=j1
                endif
160          continue
!
            zr(lwr+zi(lindfr+i1-1)-1)=1.d-16
            zr(lwi+zi(lindfr+i1-1)-1)=1.d-16
150      continue
!
!-- RAJOUTER LA SELECTION DES DDL
!-- ON GARDE LES 6 DDL POUR LA CONSTRUCTION DU MODELE D'INTERFACE
!
!
!-- POUR L'EXPANSION : ON NE GARDE EN SORTIE QUE LE SEV SUR L'INTERFACE
        if (switch .eq. 0) then
!
            call wkvect(matmod, 'V V R', nddlin*coeff, lmatmo)
!
            do 175 l1 = 1, coeff
                j1=zi(lindfr+l1-1)
                do 185 k1 = 1, nbvect
                    temp=zr(lmolor+(j1-1)*nbvect+k1-1)
                    do 195 i1 = 1, nddlin
                        m1=zi(lintrf+i1-1)
                        zr(lmatmo+(l1-1)*nddlin+i1-1)= zr(lmatmo+(l1-&
                        1)*nddlin+i1-1) +zr(lkryl+(k1-1)*6*nnoint+m1-&
                        1)*temp
195                  continue
185              continue
175          continue
            nbmod=coeff
            goto 9999
!
        endif
!
!-- CONSTRUCTION DU SOUS ESPACE POUR LE PROBLEME COMPLET
        decal=int((n1-1)*coeff*neq)
        do 170 l1 = 1, coeff
            j1=zi(lindfr+l1-1)
            do 180 k1 = 1, nbvect
                temp=zr(lmolor+(j1-1)*nbvect+k1-1)
                do 190 i1 = 1, nddlin
                    m1=zi(lintrf+i1-1)
!
                    zr(lmakry+decal+(l1-1)*neq+zi(linlag+(i1-1)*2)-1)=&
                    zr(lmakry+decal+(l1-1)*neq+zi(linlag+(i1-1)*2)-1)&
                    +zr(lkryl+(k1-1)*6*nnoint+m1-1)*temp
!
                    zr(lmakry+decal+(l1-1)*neq+zi(linlag+(i1-1)*2+1)-&
                    1)= zr(lmakry+decal+(l1-1)*neq+zi(linlag+(i1-1)*2+&
                    1)-1) +zr(lkryl+(k1-1)*6*nnoint+m1-1)*temp
190              continue
180          continue
170      continue
!
70  end do
!
!
!
!-- RELEVE STATIQUE DU SOUS ESPACE DE KRYLOV SUR LE MODELE COMPLET
    call dismoi('F', 'SOLVEUR', raide, 'MATR_ASSE', ibid,&
                solveu, ibid)
    call resoud(raide, '&&MOIN93.MATPRE', solveu, ' ', nsekry,&
                ' ', ' ', ' ', zr(lmakry), cbid,&
                ' ', .true., 0, iret)
!
!---------------------------------------------C
!--                                         --C
!-- PROJECTION DE LA MASSE ET DE LA RAIDEUR --C
!--                                         --C
!---------------------------------------------C
!
    call wkvect('&&MODINT.M_PROJ_TEMP', 'V V R', neq*nsekry, lmatrm)
    call wkvect('&&MODINT.K_PROJ_TEMP', 'V V R', neq*nsekry, lmatrk)
    call wkvect('&&MODINT.M_PROJ', 'V V R', nsekry**2, lmapro)
    call wkvect('&&MODINT.K_PROJ', 'V V R', nsekry**2, lkpro)
!
!-- MISE A 0 DES DDL DE LAGRANGE
    call dismoi('F', 'NOM_NUME_DDL', raide, 'MATR_ASSE', ibid,&
                nume, ibid)
    call dismoi('F', 'PROF_CHNO', nume, 'NUME_DDL', ibid,&
                prno, ibid)
    call jeveuo(prno//'.DELG', 'L', ldelg)
!
    do 200 i1 = 1, neq
        if (zi(ldelg+i1-1) .lt. 0) then
            do 210 j1 = 1, nsekry
                zr(lmakry+(j1-1)*neq+i1-1)=0.d0
210          continue
        endif
200  end do
!
    call jeveuo(masse(1:19)//'.&INT', 'L', lmatm)
    call mrmult('ZERO', lmatm, zr(lmakry), zr(lmatrm), nsekry,&
                .true.)
    call jeveuo(raide(1:19)//'.&INT', 'L', lmatk)
    call mrmult('ZERO', lmatk, zr(lmakry), zr(lmatrk), nsekry,&
                .true.)
!
    do 240 j1 = 1, nsekry
        zr(lmapro+(j1-1)*nsekry+j1-1)= ddot(neq,zr(lmakry+(j1-1)*neq),&
        1, zr(lmatrm+(j1-1)*neq),1)
        zr(lkpro+(j1-1)*nsekry+j1-1)= ddot(neq,zr(lmakry+(j1-1)*neq),&
        1, zr(lmatrk+(j1-1)*neq),1)
        do 250 i1 = 1, j1-1
            zr(lmapro+(j1-1)*nsekry+i1-1)= ddot(neq,zr(lmakry+(i1-1)*&
            neq),1, zr(lmatrm+(j1-1)*neq),1)
            zr(lmapro+(i1-1)*nsekry+j1-1)= zr(lmapro+(j1-1)*nsekry+i1-&
            1)
!
            zr(lkpro+(j1-1)*nsekry+i1-1)= ddot(neq,zr(lmakry+(i1-1)*&
            neq),1, zr(lmatrk+(j1-1)*neq),1)
            zr(lkpro+(i1-1)*nsekry+j1-1)= zr(lkpro+(j1-1)*nsekry+i1-1)
250      continue
240  end do
!
!-------------------------------------------------C
!--                                             --C
!-- RESOLUTION DU PB AUX VALEURS PROPRES REDUIT --C
!--                                             --C
!-------------------------------------------------C
!
    call wkvect('&&MODINT.VECT_ALPHAR', 'V V R', nsekry, lalpr)
    call wkvect('&&MODINT.VECT_ALPHAI', 'V V R', nsekry, lalpi)
    call wkvect('&&MODINT.VECT_BETA', 'V V R', nsekry, lbeta)
    call wkvect('&&MODINT.MATR_MOD_RED', 'V V R', nsekry**2, lmored)
!
    call dggev('N', 'V', nsekry, zr(lkpro), nsekry,&
               zr(lmapro), nsekry, zr(lalpr), zr(lalpi), zr(lbeta),&
               zr(lmored), nsekry, zr(lmored), nsekry, swork,&
               -1, info)
    lwork=int(swork(1))
    call wkvect('&&MODINT.MATR_WORK_DGGEV', 'V V R', lwork, llwork)
    call dggev('N', 'V', nsekry, zr(lkpro), nsekry,&
               zr(lmapro), nsekry, zr(lalpr), zr(lalpi), zr(lbeta),&
               zr(lmored), nsekry, zr(lmored), nsekry, zr(llwork),&
               lwork, info)
!-- ON REACTIVE LE TEST FPE
    call matfpe(1)
!
!-- CLASSEMENT DES FREQUENCES PROPRES
    temp=1.d+16
    do 260 i1 = 1, nsekry
        if (abs(zr(lbeta+i1-1)) .gt. 0) then
            lambda=zr(lalpr+i1-1)/zr(lbeta+i1-1)
            zr(lfreq+i1-1)=(sqrt(abs(lambda)))/2/pi
        else
            zr(lfreq+i1-1)=temp
        endif
260  end do
!
!
    call wkvect(vefreq, 'V V R', nbmod, lvp)
!
    do 270 i1 = 1, nbmod
        temp=1.d+16
        do 280 j1 = 1, nsekry
            if (zr(lfreq+j1-1) .lt. temp) then
                temp=zr(lfreq+j1-1)
                zi(lindfr+i1-1)=j1
            endif
280      continue
        zr(lfreq+zi(lindfr+i1-1)-1)=1.d+16
        lambda=zr(lalpr+zi(lindfr+i1-1)-1)/ zr(lbeta+zi(lindfr+i1-1)-&
        1)-0*shift
        zr(lvp+i1-1)=(sqrt(abs(lambda)))/2/pi
270  end do
!
!-------------------------------------------------------C
!--                                                   --C
!-- RESTITUTION DES MODES D'INTERFACE SUR LE MAILLAGE --C
!--                                                   --C
!-------------------------------------------------------C
!
    call wkvect(matmod, 'V V R', neq*nbmod, lmatmo)
    do 290 j1 = 1, nbmod
        do 300 k1 = 1, nsekry
            temp=zr(lmored+(zi(lindfr+j1-1)-1)*nsekry+k1-1)
            do 310 i1 = 1, neq
                zr(lmatmo+(j1-1)*neq+i1-1)=zr(lmatmo+(j1-1)*neq+i1-1)+&
                temp*zr(lmakry+(k1-1)*neq+i1-1)
310          continue
300      continue
290  end do
!
!---------------------------------------C
!--                                   --C
!-- DESTRUCTION DES OBJETS DE TRAVAIL --C
!--                                   --C
!---------------------------------------C
!
    call jedetr('&&MODINT.M_PROJ_TEMP')
    call jedetr('&&MODINT.K_PROJ_TEMP')
    call jedetr('&&MODINT.M_PROJ')
    call jedetr('&&MODINT.K_PROJ')
    call jedetr('&&MODINT.INTERFACES_SST')
!
    call jedetr('&&MODINT.VECT_ALPHAR')
    call jedetr('&&MODINT.VECT_ALPHAI')
    call jedetr('&&MODINT.VECT_BETA')
    call jedetr('&&MODINT.MATR_MOD_RED')
    call jedetr('&&MODINT.MATR_WORK_DGGEV')
!
9999  continue
!
    call detrsd('MATR_ASSE', imped)
!
    call jedetr('&&MODINT.VECT_TEMP')
    call jedetr('&&MODINT.VECT_TEMP_2')
    call jedetr('&&MODINT.KRYLOV_INT')
    call jedetr('&&MODINT.SE_KRYLOV')
    call jedetr('&&MODINT.HESSENBERG')
!
    call jedetr('&&MODINT.LEFT_MODES')
    call jedetr('&&MODINT.RIGHT_MODES')
    call jedetr('&&MODINT.REAL_PART')
    call jedetr('&&MODINT.IMAG_PART')
    call jedetr('&&MODINT.V_F_PRO')
    call jedetr('&&MODINT.V_IND_F_PRO')
!
!-- FIN --C
!
    call jedema()
end subroutine
