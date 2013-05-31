subroutine fetacc(option, rang, dimtet, imsmi, imsmk,&
                  nbreoa, itps, irg, irr, ivlagi,&
                  nbi, ir1, ir2, ir3, nomggt,&
                  lrigid, dimgi, sdfeti, ipiv, nbsd,&
                  vsdf, vddl, matas, nomgi, lstogi,&
                  infofe, irex, iprj, nbproc)
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
!    - FONCTION REALISEE:  PROCEDURE D'ACCELERATION DE ALFETI
!   IN OPTION  : IN : 1 -> MISE A JOUR LAMBDA0, R0 ET G0 POUR ACSM
!                     2 -> MISE A JOUR DU RESIDU PROJETE SCALE HK_TILDE
!                          PRECONDITIONNE
!                          INPUT ZR(IR1)= A*GK, ZR(IR2)=M-1*A*GK
!                          OUTPUT ZR(IR2)= ZR(IR2) + CORRECTION
!   IN RANG    : IN : RANG DU PROCESSUS
!   IN DIMTET  : IN : DIM MAX DE L'ESPACE DE PROJECTION
!   IN IMSMI/K : IN : ADRESSES JEVEUX OBJETS DE REORTHO
!                     '&FETI.MULTIPLE.SM.K24'/'IN'
!   IN NBREOA  : IN : NOMBRE DE PAS DE TEMPS EFFECTIF A REORTHOGONALISER
!   IN ITPS    : IN : INDICE DU PAS DE TEMPS COURANT
!   IN/OUT IRG/IRR/IVLAGI : IN : ADRESSES G0, R0 ET LAMBDA0
!   IN NBI     : IN : TAILLE DU PROBLEME D'INTERFACE
!   IN/OUT IR1/IR2/IR3 : IN : ADRESSES JEVEUX DE VECTEURS AUXILIAIRES
!                    DE TAILLE NBI
! ARGUMENTS USUELS DE FETPRJ
!   IN NOMGGT/LRIGID/DIMGI/SDFETI/IPIV/NBSD/VSDF/VDDL/MATAS/NOMGI
!      LSTOGI/INFOFE/IREX/IPRJ/NBPROC
!----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
! aslint: disable=W1304,W1504
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
!
    include 'asterfort/fetprj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/ddot.h'
    include 'blas/dgemv.h'
    integer :: option, rang, dimtet, imsmi, imsmk, nbreoa, itps, irg, irr
    integer :: ivlagi, nbi, ir1, ir2, ir3, dimgi, ipiv, nbsd, vsdf(nbsd)
    integer :: vddl(nbsd), irex, iprj, nbproc
    character(len=19) :: sdfeti, matas
    character(len=24) :: nomggt, nomgi, infofe
    logical :: lrigid, lstogi
!
!
! DECLARATION VARIABLES LOCALES
    integer :: imsmr, i, nbddsm, ipsro, iddro, iddfro, j, iaux1, ifm, niv
    integer :: imsmr1, imsmr2, imsmr3, imsmr4, nbi1, j1
    character(len=8) :: k8bid1
    character(len=24) :: k24bid
    integer(kind=4) :: nbi4, nb4
!
! CORPS DU PROGRAMME
    call jemarq()
    call infniv(ifm, niv)
    nbi1=nbi-1
    nbi4=nbi
    if (option .eq. 1) then
!
!  ON REMET A JOUR LAMBDA0, RO ET G0 SI TAILLE ESPACE DE PROJECTION NON
!  NUL ET PROC. 0
        if ((rang.eq.0) .and. (dimtet.ne.0)) then
!  CALCUL DU TERME CORRECTIF A AJOUTER A G0 (ZR(IRG))
            call wkvect('&&FETI.MSM.R1', 'V V R', dimtet, imsmr)
            do 30 i = 1, nbreoa
                nbddsm=zi(imsmi-1+i)
                nb4=nbddsm
!  POUR SAUTER LES PAS DE TEMPS SANS INFORMATION
                if (nbddsm .ne. 0) then
                    k8bid1=zk24(imsmk-1+i)(1:8)
                    call jeveuo('&&FETI.PS.'//k8bid1, 'L', ipsro)
                    call jeveuo('&&FETI.DD.'//k8bid1, 'L', iddro)
                    call jeveuo('&&FETI.FIDD.'//k8bid1, 'L', iddfro)
                    do 20 j = 1, nbddsm
                        zr(imsmr+j-1)=ddot(nbi,zr(iddro+j*nbi),1,zr(&
                        irg),1)/ zr(ipsro+j)
20                  continue
!  MISE A JOUR DU RESIDU ET RESIDU PROJETE
                    iaux1=iddfro+nbi
                    call dgemv('N', nbi4, nb4, 1.d0, zr(iaux1),&
                               nbi4, zr( imsmr), 1, 0.d0, zr(ir1),&
                               1)
                    call daxpy(nbi4, -1.d0, zr(ir1), 1, zr(irr),&
                               1)
                    k24bid(1:4)='VIDE'
                    call fetprj(nbi, zr(ir1), zr(ir2), nomggt, lrigid,&
                                dimgi, 1, sdfeti, ipiv, nbsd,&
                                vsdf, vddl, matas, nomgi, lstogi,&
                                infofe, irex, iprj, nbproc, rang,&
                                k24bid)
                    call daxpy(nbi4, -1.d0, zr(ir2), 1, zr(irg),&
                               1)
!  MISE A JOUR DU LAGRANGE INITIAL
                    iaux1=iddro+nbi
                    call dgemv('N', nbi4, nb4, 1.d0, zr(iaux1),&
                               nbi4, zr( imsmr), 1, 1.d0, zr(ivlagi),&
                               1)
                    call jelibe('&&FETI.PS.'//k8bid1)
                    call jelibe('&&FETI.DD.'//k8bid1)
                    call jelibe('&&FETI.FIDD.'//k8bid1)
                endif
30          continue
            call jedetr('&&FETI.MSM.R1')
            if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/FETACC', rang,&
                                      '> ACCELERATION_SM MAJ G0/R0/LAMBDA0'
!  FIN DU IF RANG ET DIMTET
        endif
!
    else if (option.eq.2) then
!
!  ON REMET A JOUR HK_TILDE SI TAILLE ESPACE DE PROJECTION NON
!  NUL ET PROC. 0
        if ((rang.eq.0) .and. (dimtet.ne.0)) then
!
            call wkvect('&&FETI.MMA.R1', 'V V R', nbi, imsmr1)
            call wkvect('&&FETI.MMA.R2', 'V V R', nbi, imsmr2)
            call wkvect('&&FETI.MMA.R3', 'V V R', dimtet, imsmr3)
            call wkvect('&&FETI.MMA.R4', 'V V R', dimtet, imsmr4)
            do 50 i = 1, nbreoa
                nbddsm=zi(imsmi-1+i)
                nb4=nbddsm
!  POUR SAUTER LES PAS DE TEMPS SANS INFORMATION
                if (nbddsm .ne. 0) then
                    k8bid1=zk24(imsmk-1+i)(1:8)
                    call jeveuo('&&FETI.PS.'//k8bid1, 'L', ipsro)
                    call jeveuo('&&FETI.DD.'//k8bid1, 'L', iddro)
                    call jeveuo('&&FETI.FIDD.'//k8bid1, 'L', iddfro)
                    if (i .ne. 1) then
! SOMME M-1*GK + SIGMA J=1 A I-1: VJ * THETAJ
                        do 40 j = 0, nbi1
                            zr(imsmr2+j)=zr(imsmr1+j)+zr(ir2+j)
40                      continue
                    else
! INIT VECTEUR POUR STOCKER SIGMA I=NBREOA A ITPS: VI * THETAI
                        do 42 j = 0, nbi1
                            zr(imsmr1+j)=0.d0
42                      continue
                        call dcopy(nbi4, zr(ir2), 1, zr(imsmr2), 1)
                    endif
                    do 43 j = 1, dimtet
                        zr(imsmr3+j-1)=0.d0
                        zr(imsmr4+j-1)=0.d0
43                  continue
! PRODUIT MATRICE-VECTEUR (AI*VI)T * SOMME PRECEDENTE
                    iaux1=iddfro+nbi
                    call dgemv('T', nbi4, nb4, 1.d0, zr(iaux1),&
                               nbi4, zr(imsmr2), 1, 0.d0, zr(imsmr3),&
                               1)
! PRODUIT MATRICE-VECTEUR VIT * GK
                    iaux1=iddro+nbi
                    call dgemv('T', nbi4, nb4, 1.d0, zr(iaux1),&
                               nbi4, zr(ir1), 1, 0.d0, zr(imsmr4),&
                               1)
! THETAI
                    do 45 j = 1, nbddsm
                        j1=j-1
                        zr(imsmr3+j1)=(zr(imsmr4+j1)-zr(imsmr3+j1))/&
                        zr(ipsro+j)
45                  continue
! PRODUIT MATRICE-VECTEUR VI * THETAI ET MISE A JOUR STOCKAGE
                    iaux1=iddro+nbi
                    call dgemv('N', nbi4, nb4, 1.d0, zr(iaux1),&
                               nbi4, zr(imsmr3), 1, 1.d0, zr(imsmr1),&
                               1)
                    call jelibe('&&FETI.PS.'//k8bid1)
                    call jelibe('&&FETI.DD.'//k8bid1)
                    call jelibe('&&FETI.FIDD.'//k8bid1)
                endif
50          continue
! MISE A JOUR VECTEUR SOLUTION
            call daxpy(nbi4, 1.d0, zr(imsmr1), 1, zr(ir2),&
                       1)
            call jedetr('&&FETI.MMA.R1')
            call jedetr('&&FETI.MMA.R2')
            call jedetr('&&FETI.MMA.R3')
            call jedetr('&&FETI.MMA.R4')
            if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/FETACC', rang,&
                                      '> ACCELERATION_MA MAJ'// ' HK_TILDE'
!  FIN DU IF RANG ET DIMTET
        endif
!
    else
        call u2mess('F', 'ALGORITH3_61')
!  FIN DU IF OPTION
    endif
    call jedema()
end subroutine
