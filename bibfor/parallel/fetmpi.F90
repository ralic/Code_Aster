subroutine fetmpi(optmpi, nbsd, ifm, niv, rang,&
                  nbproc, ach24, ach241, ach242, argr1)
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
#include "aster_types.h"
#include "asterf.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterc/loisem.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/gcncon.h"
#include "asterfort/getvis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerazo.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
#include "asterfort/utimsd.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
    integer :: optmpi, ifm, niv, nbsd, nbproc, rang
    character(len=24) :: ach24, ach241, ach242
    real(kind=8) :: argr1
! person_in_charge: olivier.boiteau at edf.fr
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  APPELS MPI POUR FETI
!
! ARGUMENTS D'APPELS
! IN OPTMPI  : OPTION DE LA ROUTINE (CHANGER NBOPT DANS LE SOURCE SI
!       VOUS MODIFIER LE NBRE D'OPTIONS)
!    =1        CREATION OBJET JEVEUX  '&FETI.LISTE.SD.MPI' POUR BOUCLE
!          SUR LES SOUS-DOMAINES PARALLELISEE
!    =2        RANG DU PROCESSUS
!    =3        NOMBRE DE PROCESSEURS
!    =4 OU 5   REDUCTION SUR L'OBJET ACH24 (MPI_INTEGER OU MPI_DOUBLE
!          _PRECISION AVEC SUBTILITE QUANT AU NBRE DE DONNEES)
!    =6        REDUCTION PUIS DIFFUSION SUR L'OBJET ACH24
!    =7        REDUCTION SUR L'OBJET ACH24 EN MPI_DOUBLE_PRECISION
!    =8        COLLECTE SELECTIVE DE L'OBJET ACH24 PAR UN MPI_GATHERV
!    =9        DISTRIBUTION DE L'OBJET ACH24
!    =10       DISTRIBUTION DU REEL ARGR1
!    =71       IDEM QUE 7 PUIS DIFFUSION A TOUS LES PROCS
! IN NBSD    : NOMBRE DE SOUS-DOMAINES SI OPTMI=1
!          TAILLE DU MPI_** SI OPTMPI=4,5,6,7, 71 OU 9
! IN IFM,NIV : NIVEAU D'IMPRESSION (SI NIV.GE.2 IMPRESSION, DOIT ETRE
!          CONTROLE PAR INFOFE(10:10)
! IN  ACH24  : ARGUMENT CH24 POUR OPTMPI=4,5,6,7,71,8 OU 9
! IN  ACH241 : ARGUMENT CH24 POUR OPTMPI=8
! IN  ACH242 : ARGUMENT CH24 POUR OPTMPI=8
! IN  ARGR1  : ARGUMENR REEL POUR OPTMPI=10
! IN/OUT RANG: RANG DU PROCESSUS SI OPTMPI=1,2 OU 8
! IN/OUT NBPROC : RANG DU PROCESSUS SI OPTMPI=1,3 OU 8
!----------------------------------------------------------------------
!
!
#ifdef _USE_MPI
#include "mpif.h"
!
! DECLARATION VARIABLES LOCALES
    integer :: iaux1, idd, nbsd1, ilist, iexist, ired, i, iach
    integer :: iaux2, iaux3, iaux4, decal, ibid, iproc2
    integer :: ilist1, iproc, nbpro1, iproc1, nbsdp0, iach1, iach2
    integer :: imon, nbopt, opt, iarg
    mpi_int :: mpicou, nbpro4, rang4, nbsd4, iermpi, nbsd41, lr8, lint
    character(len=8) :: k8bid
    character(len=24) :: nom1, nomlog, nomlo1, nommon
    real(kind=8) :: temps(6)
!
    call jemarq()
! --- COMMUNICATEUR MPI DE TRAVAIL
    call asmpi_comm('GET', mpicou)
! INITS.
    if (loisem() .eq. 8) then
        lint=MPI_INTEGER8
    else
        lint=MPI_INTEGER
    endif
    lr8 = MPI_DOUBLE_PRECISION
    nbsd4=nbsd
! NBRE D'OPTION DE FETMPI
    nbopt=11*2
! POUR LE MONITORING DU PARALLELISME
    if (niv .ge. 2) then
        nommon='&FETI.MONITORING.MPI'
        call jeexin(nommon, iexist)
        if (iexist .eq. 0) then
            call wkvect(nommon, 'V V R', nbopt, imon)
            call jerazo(nommon, nbopt, 1)
        else
            call jeveuo(nommon, 'E', imon)
        endif
    endif
! RANG DU PROCESSEUR POUR LIMITER LES AFFICHAGES
    if (niv .ge. 2) then
        call asmpi_info(mpicou, rank=rang4)
        rang=rang4
        if (rang .ne. 0) niv=0
    endif
!
!---------------------------------------------- OPTION = 1
    if (optmpi .eq. 1) then
! POUR LE MONITORING DU PARALLELISME
        if (niv .ge. 2) then
            call uttcpu('CPU.FETMPI.1', 'INIT', ' ')
            call uttcpu('CPU.FETMPI.1', 'DEBUT', ' ')
        endif
        nbsd1=nbsd+1
! OBJET TEMPORAIRE POUR PARALLELISME MPI:
! ZI(ILIST+I)=1 IEME SOUS-DOMAINE CALCULE PAR PROCESSEUR COURANT
!            =0 ELSE
        nomlog='&FETI.LISTE.SD.MPI'
        call jeexin(nomlog, iexist)
! SI L OBJET NOMLOG EXISTE DEJA : ARRET
        ASSERT(iexist.eq.0)
        call wkvect(nomlog, 'V V I', nbsd1, ilist)
! OBJET TEMPORAIRE POUR PARALLELISME MPI:
! ZI(ILIST1+I-1)=NUMERO DU PROCESSEUR QUI LE CONCERNE
        nomlo1='&FETI.LISTE.SD.MPIB'
        call jeexin(nomlo1, iexist)
! SI L OBJET NOMLO1 EXISTE DEJA : ARRET
        ASSERT(iexist.eq.0)
        call wkvect(nomlo1, 'V V I', nbsd, ilist1)
!
        call asmpi_info(mpicou, size=nbpro4)
        nbproc=nbpro4
        call asmpi_info(mpicou, rank=rang4)
        rang=rang4
        if (nbproc .gt. 1) then
! ON EST EN PARALLELE, L'UTILISATEUR A PEUT-ETRE EMIS UN SOUHAIT QUANT
! AU NBRE DE SD POUR LE PROCESSEUR MAITRE
            call getvis(ach24(1:16), 'NB_SD_PROC0', iocc=1, scal=nbsdp0, nbret=ibid)
            if ((nbsd-nbsdp0) .lt. (nbproc-1)) call u2mess('F', 'APPELMPI_3')
        else
            nbsdp0=0
        endif
! DECOUPAGE DU TRAVAIL PAR PROCESSEUR. POUR FACILITER LE TRAVAIL DES
! ENVOI DE MODES RIGIDES ON PREFERE GROUPER LES SD PAR SOUS-DOMAINES
! D'AUTRE PART, ON SOULAGE D'UN POINT DE VUE MEMOIRE, SI POSSIBLE LE
! PROCESSEUR DE RANG 0 QUI AURA AUSSI A STOCKER GI, GIT*GI ET LES VEC
! TEURS DE REORTHO. DONC, SI NBPROC < NBSD, ON REDISTRIBUE LES SD COMP
! LEMENTAIRES EN COMMENCANT PAR LE PROC 1
! EXEMPLE: 8 SD ET 4 PROC
! PROC 0 : SD1/SD2
! PROC 1 : SD3/SD4
! ...
! EXEMPLE: 9 SD ET 4 PROC
! PROC 0 : SD1/2
! PROC 1 : SD3/SD4/SD5
! PROC 2 : SD6/SD7
!...
! DOMAINE GLOBALE (IDD=0) CONCERNE TOUS LES PROCESSEURS
        zi(ilist)=1
        do 90 idd = 1, nbsd
            zi(ilist+idd)=0
            zi(ilist1+idd-1)=0
90      continue
        if (nbsdp0 .eq. 0) then
            iaux1=nbsd/nbproc
            iaux4=nbsd-(nbproc*iaux1)
            iproc2=0
        else
! ATTRIBUTIONS DU PROC 0
            do 92 idd = 1, nbsdp0
                if (rang .eq. 0) zi(ilist+idd)=1
                zi(ilist1+idd-1)=0
92          continue
! RESTE AUX AUTRES PROC
            iaux1=(nbsd-nbsdp0)/(nbproc-1)
            iaux4=(nbsd-nbsdp0)-((nbproc-1)*iaux1)
            iproc2=1
        endif
        nbpro1=nbproc-1
        do 100 iproc = iproc2, nbpro1
! INDICE RELATIF DU PROCESSEUR A EQUILIBRER
            iproc1=iproc-iproc2
! BORNES DES SDS A LUI ATTRIBUER
            iaux2=1+nbsdp0+iproc1*iaux1
            iaux3=nbsdp0+(iproc1+1)*iaux1
! CALCUL D'UN DECALAGE EVENTUEL DU AU RELIQUAT DE SD
            if (iaux4 .lt. iproc1) then
                decal=iaux4
            else
                if (iproc1 .eq. 0) then
                    decal=0
                else
                    decal=iproc1-1
                    iaux3=iaux3+1
                endif
            endif
! ATTRIBUTIONS SD/PROC
            do 95 idd = iaux2, iaux3
                if (iproc .eq. rang) zi(ilist+decal+idd)=1
                zi(ilist1+decal+idd-1)=iproc
95          continue
100      continue
!
! MONITORING
        if (niv .ge. 2) then
            call uttcpu('CPU.FETMPI.1', 'FIN', ' ')
            call uttcpr('CPU.FETMPI.1', 6, temps)
            zr(imon+2*(optmpi-1)) =zr(imon+2*(optmpi-1)) +temps(5)
            zr(imon+2*(optmpi-1)+1)=zr(imon+2*(optmpi-1)+1)+temps(6)
            call utimsd(ifm, 2, .false., .true., nomlog(1:19),&
                        1, 'V')
            call utimsd(ifm, 2, .false., .true., nomlo1(1:19),&
                        1, 'V')
            write(ifm,*)'<FETI/FETMPI> REPARTITION SD/PROC RANG/NBPROC ',&
     &                rang,nbproc
        endif
!
!---------------------------------------------- OPTION = 2
    else if (optmpi.eq.2) then
        if (niv .ge. 2) then
            call uttcpu('CPU.FETMPI.2', 'INIT', ' ')
            call uttcpu('CPU.FETMPI.2', 'DEBUT', ' ')
        endif
! DETERMINATION DU RANG D'UN PROCESSUS (RANG)
        call asmpi_info(mpicou, rank=rang4)
        rang=rang4
! MONITORING
        if (niv .ge. 2) then
            call uttcpu('CPU.FETMPI.2', 'FIN', ' ')
            call uttcpr('CPU.FETMPI.2', 6, temps)
            zr(imon+2*(optmpi-1)) =zr(imon+2*(optmpi-1)) +temps(5)
            zr(imon+2*(optmpi-1)+1)=zr(imon+2*(optmpi-1)+1)+temps(6)
            write(ifm,*)'<FETI/FETMPI> RANG ',rang
        endif
!
!---------------------------------------------- OPTION = 3
    else if (optmpi.eq.3) then
        if (niv .ge. 2) then
            call uttcpu('CPU.FETMPI.3', 'INIT', ' ')
            call uttcpu('CPU.FETMPI.3', 'DEBUT', ' ')
        endif
! DETERMINATION DU NOMBRE DE PROCESSEURS (NBPROC)
        call asmpi_info(mpicou, size=nbpro4)
        nbproc=nbpro4
! MONITORING
        if (niv .ge. 2) then
            call uttcpu('CPU.FETMPI.3', 'FIN', ' ')
            call uttcpr('CPU.FETMPI.3', 6, temps)
            zr(imon+2*(optmpi-1)) =zr(imon+2*(optmpi-1)) +temps(5)
            zr(imon+2*(optmpi-1)+1)=zr(imon+2*(optmpi-1)+1)+temps(6)
            write(ifm,*)'<FETI/FETMPI> RANG/NBPROC ',rang,nbproc
        endif
!
!---------------------------------------------- OPTION = 4,5,6,7,71
        else if (((optmpi.ge.4).and.(optmpi.le.7)).or. (optmpi.eq.71))&
    then
        if (niv .ge. 2) then
            call uttcpu('CPU.FETMPI.4', 'INIT', ' ')
            call uttcpu('CPU.FETMPI.4', 'DEBUT', ' ')
        endif
! REDUCTION DU VECTEUR ACH24 POUR LE PROCESSEUR MAITRE
! PAR EXEMPLE, L'OBJET JEVEUX MATR_ASSE.FETH
        call jeveuo(ach24, 'E', iach)
        call gcncon('.', k8bid)
        nom1='&&REDUCE'//k8bid
        nbsd1=nbsd-1
        if ((optmpi.eq.4) .or. (optmpi.eq.6)) then
            call wkvect(nom1, 'V V I', nbsd, ired)
            do 400 i = 0, nbsd1
                zi(ired+i)=zi(iach+i)
400          continue
            else if ((optmpi.eq.5).or.(optmpi.eq.7).or.(optmpi.eq.71))&
        then
            call wkvect(nom1, 'V V R', nbsd, ired)
            do 402 i = 0, nbsd1
                zr(ired+i)=zr(iach+i)
402          continue
        endif
        if (optmpi .eq. 4) then
            call MPI_REDUCE(zi(ired), zi(iach), nbsd4, lint, MPI_SUM,&
                            0, mpicou, iermpi)
        else if (optmpi.eq.5) then
! PROFILER POUR LES OBJETS JEVEUX '&FETI.INFO.CPU' QUI NE DOIVENT PAS
! CONTENIR L'INFO REDONDANTE DU PREMIER INDICE (TEMPS CONCERNANT LA MAT
! RICE GLOBALE COMMUNE A TOUS LES PROCESSEURS)
            nbsd41=nbsd4-1
            call MPI_REDUCE(zr(ired+1), zr(iach+1), nbsd41, lr8, MPI_SUM,&
                            0, mpicou, iermpi)
        else if (optmpi.eq.6) then
            call MPI_ALLREDUCE(zi(ired), zi(iach), nbsd4, lint, MPI_SUM,&
                               mpicou, iermpi)
        else if (optmpi.eq.7) then
            call MPI_REDUCE(zr(ired), zr(iach), nbsd4, lr8, MPI_SUM,&
                            0, mpicou, iermpi)
        else if (optmpi.eq.71) then
            call MPI_ALLREDUCE(zr(ired), zr(iach), nbsd4, lr8, MPI_SUM,&
                               mpicou, iermpi)
        endif
        call jedetr(nom1)
!
! MONITORING
        if (niv .ge. 2) then
            call uttcpu('CPU.FETMPI.4', 'FIN', ' ')
            call uttcpr('CPU.FETMPI.4', 6, temps)
! GLUTE STUPIDE POUR NE PAS CHANGER LE  EN 11
            if (optmpi .eq. 71) then
                opt=11
            else
                opt=optmpi
            endif
            zr(imon+2*(opt-1)) =zr(imon+2*(opt-1)) +temps(5)
            zr(imon+2*(opt-1)+1)=zr(imon+2*(opt-1)+1)+temps(6)
            if ((optmpi.ne.6) .and. (optmpi.ne.71)) then
                write(ifm,*)'<FETI/FETMPI> RANG/MPI_REDUCE ENTIER',&
                rang,ach24
            else
                write(ifm,*)'<FETI/FETMPI> RANG/MPI_ALLREDUCE REEL',&
                rang,ach24
            endif
        endif
!
!---------------------------------------------- OPTION = 8
    else if (optmpi.eq.8) then
        if (niv .ge. 2) then
            call uttcpu('CPU.FETMPI.5', 'INIT', ' ')
            call uttcpu('CPU.FETMPI.5', 'DEBUT', ' ')
        endif
! COLLECTE SELECTIVE DU VECTEUR ACH24 POUR LE PROCESSEUR MAITRE
        call jeveuo(ach241, 'E', iach1)
        call jeveuo(ach242, 'E', iach2)
        call jeveuo(ach24, 'E', iach)
        if (nbsd .ne. 0) then
            call gcncon('.', k8bid)
            nom1='&&GATHERV'//k8bid
            call wkvect(nom1, 'V V R', nbsd, ired)
            do 500 i = 1, nbsd
                zr(ired+i-1)=zr(iach+i-1)
500          continue
        endif
        call MPI_GATHERV(zr(ired), nbsd4, lr8, zr(iach), zi4(iach1),&
                         zi4(iach2), lr8, 0, mpicou, iermpi)
!
        if (nbsd .ne. 0) call jedetr(nom1)
! MONITORING
        if (niv .ge. 2) then
            call uttcpu('CPU.FETMPI.5', 'FIN', ' ')
            call uttcpr('CPU.FETMPI.5', 6, temps)
            zr(imon+2*(optmpi-1)) =zr(imon+2*(optmpi-1)) +temps(5)
            zr(imon+2*(optmpi-1)+1)=zr(imon+2*(optmpi-1)+1)+temps(6)
            write(ifm,*)'<FETI/FETMPI> RANG/MPI_GATHERV ',rang,ach24
!          CALL UTIMSD(IFM,2,.TRUE.,.TRUE.,ACH241(1:19),1,'V')
!          CALL UTIMSD(IFM,2,.TRUE.,.TRUE.,ACH242(1:19),1,'V')
        endif
!
!---------------------------------------------- OPTION = 9
    else if (optmpi.eq.9) then
        if (niv .ge. 2) then
!
            call uttcpu('CPU.FETMPI.6', 'INIT', ' ')
            call uttcpu('CPU.FETMPI.6', 'DEBUT', ' ')
        endif
! DISTRIBUTION DU VECTEUR ACH24 PAR LE PROCESSEUR MAITRE A TOUS
! LES AUTRES PROCS
        call jeveuo(ach24, 'E', iach)
        call MPI_BCAST(zr(iach), nbsd4, lr8, 0, mpicou,&
                       iermpi)
!
! MONITORING
        if (niv .ge. 2) then
            call uttcpu('CPU.FETMPI.6', 'FIN', ' ')
            call uttcpr('CPU.FETMPI.6', 6, temps)
            zr(imon+2*(optmpi-1)) =zr(imon+2*(optmpi-1)) +temps(5)
            zr(imon+2*(optmpi-1)+1)=zr(imon+2*(optmpi-1)+1)+temps(6)
            write(ifm,*)'<FETI/FETMPI> RANG/MPI_BCAST ',rang,ach24
        endif
!
!---------------------------------------------- OPTION = 10
    else if (optmpi.eq.10) then
        if (niv .ge. 2) then
            call uttcpu('CPU.FETMPI.7', 'INIT', ' ')
            call uttcpu('CPU.FETMPI.7', 'DEBUT', ' ')
        endif
! DISTRIBUTION DU REEL ARGR1 PAR LE PROCESSEUR MAITRE A TOUS
! LES AUTRES PROCS
        call MPI_BCAST(argr1, 1, lr8, 0, mpicou,&
                       iermpi)
!
! MONITORING
        if (niv .ge. 2) then
            call uttcpu('CPU.FETMPI.7', 'FIN', ' ')
            call uttcpr('CPU.FETMPI.7', 6, temps)
            zr(imon+2*(optmpi-1)) =zr(imon+2*(optmpi-1)) +temps(5)
            zr(imon+2*(optmpi-1)+1)=zr(imon+2*(optmpi-1)+1)+temps(6)
            write(ifm,*)'<FETI/FETMPI> RANG/MPI_BCAST ',rang,argr1
        endif
!
!---------------------------------------------- OPTION
! <1 OU >10 OU DIFFERENT DE 71
    else
        ASSERT(.false.)
    endif
    call jedema()
!
!
#else
!
! DECLARATION VARIABLES LOCALES
    integer :: nbsd1, ilist, idd, iexist
    character(len=24) :: nomlog
    real(kind=8) :: rdummy
!   dummy arguments
    nomlog = ach24
    nomlog = ach241
    nomlog = ach242
    rdummy = argr1
! CORPS DU PROGRAMME
    call jemarq()
!
    if (optmpi .eq. 1) then
        nbsd1=nbsd+1
        nomlog='&FETI.LISTE.SD.MPI'
        call jeexin(nomlog, iexist)
        ASSERT(iexist.eq.0)
        call wkvect(nomlog, 'V V I', nbsd1, ilist)
        do 200 idd = 0, nbsd
            zi(ilist+idd)=1
200      continue
        nomlog='&FETI.LISTE.SD.MPIB'
        call jeexin(nomlog, iexist)
        ASSERT(iexist.eq.0)
        call wkvect(nomlog, 'V V I', nbsd, ilist)
        do 201 idd = 1, nbsd
            zi(ilist+idd-1)=0
201      continue
        rang=0
        nbproc=1
! MONITORING
        if (niv .ge. 2) then
            call utimsd(ifm, 2, .false., .true., nomlog(1:19),&
                        1, 'V')
            write(ifm,*)'<FETI/FETMPI> RANG/NBPROC ',rang,nbproc
        endif
    else if (optmpi.eq.2) then
        rang=0
! MONITORING
        if (niv .ge. 2) write(ifm,*)'<FETI/FETMPI> RANG ',rang
    else if (optmpi.eq.3) then
        nbproc=1
! MONITORING
        if (niv .ge. 2) write(ifm,*)'<FETI/FETMPI> NBPROC ',nbproc
        else if (((optmpi.ge.4).and.(optmpi.le.10)).or.(optmpi.eq.71))&
    then
! MONITORING
        if (niv .ge. 2) write(ifm,*)'<FETI/FETMPI> OPTMPI=', optmpi,' VIDE '
    else
        ASSERT(.false.)
    endif
    call jedema()
#endif
end subroutine
