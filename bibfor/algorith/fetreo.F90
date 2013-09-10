subroutine fetreo(reorth, alphan, nbi, irg, iter,&
                  nbreor, irp, k24fir, k24ddr, k24psr,&
                  gs, igsmkp, rmin, irh, infofe,&
                  ifm, nbproc, rang, k24irp, itps,&
                  nbreoi, option, lacsm)
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL NOUVELLE DIRECTION DE DESCENTE
!                          REORTHOGONALISEE
!
!      IN :  REORTH :  LOG : FLAG INDIQUANT LA REOTHOGONALISATION
!     OUT :  ALPHAN :  IN  : NUME DU PARAM DE DESCENTE DE L'ITER SUIV
!      IN :   NBI   :  IN  : NOMBRE DE NOEUDS D'INTERFACE
!      IN :   IRG   :  IN  : ADRESSE JEVEUX GK+1
!      IN :   ITER  :  IN  : NUMERO D'ITERATION
!      IN :  NBREOR :  IN  : NBRE DE DD REORTHOGONALISEES
!      IN :   IRP   :  IN   : ADRESSE JEVEUX PK+1
!      IN : K24FIR/K24DDR/K24PSR   : K24 : OBJETS JEVEUX CONTENANT
!                             FI*PK, PK ET PK.FIPK
!      IN : GS/IGSMKP: LOG  : FLAG DETERMINANT LA METHODE DE REORTHO
!      IN :  RMIN   :  R8  : PLUS PETITE VALEUR REELLE DISCERNABLE
!      IN :   IRH   :  IN  : ADRESSE JEVEUX HK+1
!     IN RANG  : IN  : RANG DU PROCESSEUR
!     IN NBPROC: IN  : NOMBRE DE PROCESSEURS
!     IN K24IRP : K24 : NOM DE L'OBJET JEVEUX A DISTRIBUER EN PARALLELE
!     IN  ITPS   :  IN  : NUMERO DU PAS DE TEMPS POUR FETI
!     IN  NBREOI :  IN  : NBRE DE PAS DE TEMPS A REORTHOGONALISER
!     IN  OPTION : IN   : OPTION DE CALCUL, 1 -> REORTHO AVEC LES PAS DE
!                         TEMPS PRECEDENTS SI LACSM=TRUE,
!                         2-> IDEM + REORTHO AU SEIN DU MEME PAS
!     IN  LACSM : LOG : TRUE SI ACCELERATION_SM='OUI'
!----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
! aslint: disable=W1504
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/fetmpi.h"
#include "asterfort/jelibe.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
    integer :: nbi, iter, nbreor, irp, irg, irh, rang, nbproc, itps, nbreoi
    integer :: option
    real(kind=8) :: alphan
    logical :: reorth, gs, igsmkp, lacsm
    character(len=24) :: infofe, k24irp, k24fir, k24ddr, k24psr
!
!
! DECLARATION VARIABLES LOCALES
    real(kind=8) :: raux, normav, normap, rgskp2, betan, betad, beta, rmin, rbid
    integer :: iaux1, iaux2, iaux3, i, j, nbi1, ifm, ibid, k, nivmpi, iddfro
    integer :: iddro, imsmi, imsmk, nbreoa, ipsro, nbddsm
    character(len=8) :: k8bid
    character(len=24) :: k24b
    logical :: lpara
!
! INITS DIVERSES
    if (nbproc .eq. 1) then
        lpara=.false.
    else
        lpara=.true.
    endif
    if (infofe(10:10) .eq. 'T') then
        nivmpi=2
    else
        nivmpi=1
    endif
    nbi1=nbi-1
!
    if (rang .eq. 0) then
!
! PARAM EN DUR DE L'IGSM DE KAHN-PARLETT POUR REORTHOGONALISER LES DD
        rgskp2=0.717d0**2
! ---------------------------------------------------
! ----  PHASE DE REORTHOGONALISATION DES DD VIA UN GS, UN GSM OU UN
! ----  IGSM DE TYPE KAHN-PARLETT
! ---------------------------------------------------
        if (reorth) then
            if (infofe(1:1) .eq. 'T') then
                if (gs) then
                    write(ifm,*)'<FETI/FETREO',rang,'> REORTHO DE TYPE GS'
                else if (igsmkp) then
                    write(ifm,*)'<FETI/FETREO',rang,'> REORTHO DE TYPE IGSMKP'
                else
                    write(ifm,*)'<FETI/FETREO',rang,'> REORTHO DE TYPE GSM'
                endif
            endif
!
! -----------------------------
! CALCUL NOUVELLE DIRECTION DE DESCENTE ORTHOGONALISEE (ETAPE 1)
! (ZR(IRP)) PK+1 = HK+1 (EQUIVAUT A GK+1 SI SANS PRECOND)
! -----------------------------
            call dcopy(nbi, zr(irh), 1, zr(irp), 1)
!
! -----------------------------
! -----------------------------
! REORTHOGONALISATION STANDARD AU SEIN D'UN MEME PAS DE TEMPS
! -----------------------------
! -----------------------------
            if (option .eq. 2) then
                call jeveuo(k24psr, 'L', ipsro)
                call jeveuo(k24ddr, 'L', iddro)
                call jeveuo(k24fir, 'L', iddfro)
                if (iter .le. nbreor) then
                    iaux2=1
                    iaux3=iter
                else
                    iaux2=0
                    iaux3=nbreor
                endif
                if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/FETREO', rang,&
                                          '> AU SEIN DU PAS DE TEMPS'
! -----------------------------
! BOUCLE ET TEST IGSM DE KAHN-PARLETT
! -----------------------------
                do 60 i = iaux2, iaux3
! --------------
! CALCUL DE BETAKI=-(HK+1.(FI*PK))/(PK.(FI*PK)) (ETAPE 2.1)
! --------------
                    iaux1=iddfro+i*nbi
                    if (gs) then
                        raux=-ddot(nbi,zr(irh),1,zr(iaux1),1)/zr(&
                        ipsro+i)
                    else
                        raux=-ddot(nbi,zr(irp),1,zr(iaux1),1)/zr(&
                        ipsro+i)
                    endif
                    iaux1=iddro+i*nbi
                    if (igsmkp) normav=ddot(nbi,zr(irp),1,zr(irp),1)
! --------------
! CALCUL NOUVELLE DIRECTION DE DESCENTE ORTHOGONALISEE (ETAPE 2.2)
! (ZR(IRP)) PK+1_PRIM = PK+1 + BETAKI * PI
! --------------
                    call daxpy(nbi, raux, zr(iaux1), 1, zr(irp),&
                               1)
!
! --------------
! PREMIER TEST (ETAPE 2.3) SI IGSM DE TYPE KAHN-PARLETT
! --------------
                    if (igsmkp) then
                        normap=ddot(nbi,zr(irp),1,zr(irp),1)
                        if (normap .lt. (rgskp2 * normav)) then
! --------------
! CALCUL DE BETAKI_PRIM=-(PK+1_PRIM.(FI*PK))/(PK.(FI*PK)) (ETAPE 3.1)
! --------------
                            iaux1=iddfro+i*nbi
                            raux=-ddot(nbi,zr(irp),1,zr(iaux1),1)/zr(&
                            ipsro+i)
                            iaux1=iddro+i*nbi
! --------------
! CALCUL NOUVELLE DIRECTION DE DESCENTE ORTHOGONALISEE (ETAPE 3.2)
! (ZR(IRP)) PK+1_SEC = PK+1_PRIM + BETAKI_PRIM * PI
! --------------
                            call daxpy(nbi, raux, zr(iaux1), 1, zr(irp),&
                                       1)
! --------------
! SECOND TEST (ETAPE 3.3)
! --------------
                            normav=ddot(nbi,zr(irp),1,zr(irp),1)
                            if (normav .lt. (rgskp2 * normap)) then
                                do 58 j = 0, nbi1
                                    zr(irp+j)=0.d0
58                              continue
                                goto 61
                            endif
                        endif
                    endif
60              continue
! FIN DU IF OPTION
            endif
!
! -----------------------------
! -----------------------------
! REORTHOGONALISATION ENTRE PAS DE TEMPS (MEME ALGO QUE CI-DESSUS)
! -----------------------------
! -----------------------------
            if ((itps.gt.1) .and. (lacsm)) then
                call jeveuo('&FETI.MULTIPLE.SM.IN', 'L', imsmi)
                call jeveuo('&FETI.MULTIPLE.SM.K24', 'L', imsmk)
                if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/FETREO', rang,&
                                          '> ENTRE PAS DE TEMPS'
                nbreoa=min(itps-1,nbreoi)
! BOUCLE SUR LES PAS DE TEMPS PRECEDENTS
                do 35 i = 1, nbreoa
                    nbddsm=zi(imsmi-1+i)
! POUR SAUTER LES PAS DE TEMPS SANS INFORMATION (LAMBDAS=LAMBDA0)
                    if (nbddsm .ne. 0) then
                        k8bid=zk24(imsmk-1+i)(1:8)
                        call jeveuo('&&FETI.PS.'//k8bid, 'L', ipsro)
                        call jeveuo('&&FETI.DD.'//k8bid, 'L', iddro)
                        call jeveuo('&&FETI.FIDD.'//k8bid, 'L', iddfro)
! BOUCLE SUR LES VECTEURS DE DESCENTE RETENUS D'UN PAS DE TEMPS DONNE
                        do 30 j = 1, nbddsm
                            iaux1=iddfro+j*nbi
                            if (gs) then
                                raux=-ddot(nbi,zr(irh),1,zr(iaux1),1)/&
                                zr(ipsro+j)
                            else
                                raux=-ddot(nbi,zr(irp),1,zr(iaux1),1)/&
                                zr(ipsro+j)
                            endif
                            iaux1=iddro+j*nbi
                            if (igsmkp) normav=ddot(nbi,zr(irp),1,zr( irp),1)
                            call daxpy(nbi, raux, zr(iaux1), 1, zr(irp),&
                                       1)
                            if (igsmkp) then
                                normap=ddot(nbi,zr(irp),1,zr(irp),1)
                                if (normap .lt. (rgskp2 * normav)) then
                                    iaux1=iddfro+j*nbi
                                    raux=-ddot(nbi,zr(irp),1,zr(iaux1)&
                                    ,1)/zr(ipsro+j)
                                    iaux1=iddro+j*nbi
                                    call daxpy(nbi, raux, zr(iaux1), 1, zr(irp),&
                                               1)
                                    normav=ddot(nbi,zr(irp),1,zr(irp),&
                                    1)
                                    if (normav .lt. (rgskp2 * normap)) then
                                        do 25 k = 0, nbi1
                                            zr(irp+k)=0.d0
25                                      continue
                                        goto 61
                                    endif
                                endif
                            endif
30                      continue
                        call jelibe('&&FETI.PS.'//k8bid)
                        call jelibe('&&FETI.DD.'//k8bid)
                        call jelibe('&&FETI.FIDD.'//k8bid)
                    endif
35              continue
            endif
!
! SORTIE PREVUE POUR LE TEST 3.3
61          continue
! CALCUL DE ALPHAN = GK+1.PK+1
            alphan=ddot(nbi,zr(irg),1,zr(irp),1)
! VARIANTE CONFORME A CERTAINS PAPIERS.
!          ALPHAN=DDOT(NBI,ZR(IRG),1,ZR(IRH),1)
        else
!
! MONITORING
            if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/FETREO', rang,&
                                      '> SANS REORTHOGONALISATION'
! ---------------------------------------------------
! ----  PAS DE REORTHOGONALISATION (ELLE EST IMPLICITE)
! ---------------------------------------------------
! ON REORTHOGONALISE SEULEMENT PAR RAPPORT A LA DERNIERE DD
! CALCUL DE BETAK = HK+1.GK+1/HK.GK = BETANK/BETADK
            betan=ddot(nbi,zr(irg),1,zr(irh),1)
            betad=alphan
            if (abs(betad) .lt. rmin) then
                betad=rmin
                call u2mess('A', 'ALGORITH3_67')
            endif
            beta=betan/betad
            alphan=betan
!
! -----------------------------
! CALCUL NOUVELLE DIRECTION DE DESCENTE NON ORTHOGONALISEE
! (ZR(IRP)) PK+1 = HK+1 + BETAK+1 * PK
! -----------------------------
            do 90 i = 0, nbi1
                zr(irp+i)=zr(irh+i)+beta*zr(irp+i)
90          continue
!
        endif
! FIN DU SI RANG
    endif
! EN PARALLELE, ENVOI DE VO A TOUS LES PROC POUR PREPARER LE CALCUL
! SUIVANT, C'EST A DIRE Z = FI * V
    if (lpara) call fetmpi(9, nbi, ifm, nivmpi, ibid,&
                           ibid, k24irp, k24b, k24b, rbid)
end subroutine
