subroutine fetrex(option, idd, ni, vi, no,&
                  vo, irex)
!-----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  RESTRICTION-EXTRACTION AU SENS FETI
!
!      IN OPTION: IN   : 1 RESTRICTION, 2 EXTRACTION
!      IN    IDD: IN   : NUMERO DE SOUS-DOMAINE
!      IN     NI: IN   : NOMBRE DE DDLS DU VECTEUR INPUT
!      IN     VI: VR8  : VECTEUR INPUT DE TAILLE NI
!      IN     NO: IN   : NOMBRE DE DDLS DU VECTEUR OUTPUT
!      OUT    VO: VR8  : VECTEUR OUTPUT DE TAILLE NO
!     IN IREX  : IN    : ADRESSE DU VECTEUR AUXILAIRE EVITANT DES APPELS
!                        JEVEUX.
!----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
    include 'asterfort/u2mess.h'
    integer :: option, idd, ni, no, irex
    real(kind=8) :: vi(ni), vo(no)
!
!
! DECLARATION VARIABLES LOCALES
    integer :: iaux1, iaux2, j, ifetg, long, ifeti, icol, nddlci, nbddli, iaux21
    integer :: nddlcd, nbddld, k, j2
    real(kind=8) :: sign, raux2, rsign, un
!
! ROUTINE AVEC PEU DE MONITORING,JEVEUX ... CAR APPELLEE TRES SOUVENT
    un=1.d0
!
! INIT. VECTEUR SOLUTION
    do 10 j = 1, no
        vo(j)=0.d0
10  end do
!
! STRUCTURE DE DONNEES DE RESTRICTION/EXTRACTION DU SOUS-DOMAINE IDD
! SUR L'INTERFACE (POINT PAR POINT)
    ifeti=zi(irex)
    j=irex+1+(idd-1)*3
    ifetg=zi(j)
    long=zi(j+1)
    icol=zi(j+2)
    if ((option.eq.1) .or. (option.eq.2)) then
! ----------------------------------------------------------------------
! ----  OPERATEUR DE RESTRICTION RI/EXTRACTION (RI)T
! ----------------------------------------------------------------------
!
        do 20 j = 0, long
            j2=2*j
            iaux1=ifetg+j2
!
! INDICE DU JIEME NOEUD D'INTERFACE DU SOUS-DOMAINE IDD DANS LE VECTEUR
! D'INTERFACE .FETI
            iaux2=zi(iaux1)
!
! POUR CALCULS AUXILIAIRES
            raux2=iaux2*1.d0
            rsign=sign(un,raux2)
            iaux2=abs(iaux2)
            iaux21=ifeti+4*(iaux2-1)
! LE NBRE DE DDLS CUMULES AVANT LUI (NDDLCI)
! DANS LE VECTEUR D'INTERFACE/ SON NBRE DE DDLS (NBDDLI)
            nddlci=zi(iaux21+2)
            if (iaux2 .eq. 1) then
                nbddli=nddlci
            else
                nbddli=nddlci-zi(iaux21-2)
            endif
            nddlci=nddlci-nbddli
!
! NBRE DE DDLS CUMULES AVANT LUI DANS LE VECTEUR LOCAL AU SOUS-DOMAINE
! PROF_CHNO(IDD) (NDDLCD)/ SON NOMBRE DE DDLS (NBDDLD)
            nddlcd=zi(icol+j2)-1
            nbddld=zi(icol+j2+1)
!
! MONITORING
!            WRITE(IFM,*)IDD,ZI(IAUX21),RSIGN,NO,NI
!            WRITE(IFM,*)NDDLCI,NBDDLI,NDDLCD,NBDDLD
!
! TEST DE COHERENCE DES DONNEES INDIVIDUELLEMENT
!         TESTA=NDDLCI*NBDDLI*NDDLCD*NBDDLD
!          CALL ASSERT(TESTA.GT.0)
!
! TEST DE COHERENCE DES NOMBRES DE DDLS
!          CALL ASSERT(NBDDLI.EQ.NBDDLD)
!
            if (option .eq. 1) then
! RESTRICTION
                do 13 k = 1, nbddld
                    vo(nddlci+k)=vo(nddlci+k)+rsign*vi(nddlcd+k)
13              continue
            else
! EXTRACTION
                do 16 k = 1, nbddli
                    vo(nddlcd+k)=vo(nddlcd+k)+rsign*vi(nddlci+k)
16              continue
            endif
!
20      continue
    else
        call u2mess('F', 'ALGORITH3_61')
    endif
!
end subroutine
