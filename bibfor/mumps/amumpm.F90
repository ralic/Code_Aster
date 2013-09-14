subroutine amumpm(ldist, kxmps, kmonit, impr, ifmump,&
                  klag2, type, lmd, epsmat, ktypr,&
                  lpreco)
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
!--------------------------------------------------------------
! BUT : REMPLIR LES OBJETS F90 DE MUMPS REPRESENTANT LA MATRICE A PARTIR
!       DE CELLE DE CODE_ASTER.
!
! IN  LDIST  :  LOG   : LOGICAL MUMPS DISTRIBUE OR NOT
! IN  KXMPS  :   IN   : INDICE DE L'INSTANCE MUMPS DANS DMPS
! IN  KMONIT :  K24   : VECTEUR DE NOMS DES OBJ JEVEUX
! IN  IMPR   :  K14   : FLAG POUR IMPRESSION MATRICE
! IN  IFMUMP :   IN   : UNITE LOGIQUE POUR IMPRESSION FICHIER
! IN  KLAG2  :   K4   : PARAMETRE DE SOLVEUR/ELIM_LAGR2
! IN  TYPE   :   K1   : TYPE DU POINTEUR R OU C
! IN  LMD    :  LOG   : LOGIQUE PRECISANT SI ON EST EN MATR_DISTRIBUEE
! IN  EPSMAT :   R8   : SEUIL DE FILTRAGE DES TERMES DE LA MATRICE
! IN  KTYPR  :   K8   : TYPE DE RESOLUTION MUMPS (SYMDEF...)
! IN  LPRECO :  LOG   : MUMPS EST-IL UTILISE COMME PRECONDITIONNEUR ?
!---------------------------------------------------------------
! aslint: disable=W1501
! person_in_charge: olivier.boiteau at edf.fr
!
#include "aster_types.h"
#include "asterf.h"
#include "asterc/getres.h"
#include "asterc/r4maem.h"
#include "asterc/r4miem.h"
#include "asterc/r8maem.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "asterfort/asmpi_comm_jev.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibd.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: kxmps, ifmump
    logical :: ldist, lmd, lpreco
    real(kind=8) :: epsmat
    character(len=1) :: type
    character(len=4) :: klag2
    character(len=8) :: ktypr
    character(len=14) :: impr
    character(len=24) :: kmonit(12)
!
#ifdef _HAVE_MUMPS
#include "aster_mumps.h"
#include "mpif.h"
#include "jeveux.h"
!
!
    type (smumps_struc) , pointer :: smpsk
    type (cmumps_struc) , pointer :: cmpsk
    type (dmumps_struc) , pointer :: dmpsk
    type (zmumps_struc) , pointer :: zmpsk
    integer :: jsmdi, nsmdi, jsmhc, nsmhc, jdelg, n, n1, nz, nvale, jvale
    integer :: nlong, jvale2, nzloc, kterm, iterm, ifm, niv, k
    integer :: sym, iret, jcoll, iligl, jnulogl, ltot, iok, iok2, coltmp
    integer :: jnequ, kzero, ibid, ifiltr, vali(2), nbproc, nfilt1, nfilt2
    integer :: nfilt3, isizemu, nsizemu, rang, esizemu
    mumps_int :: nbeq, nz2, iligg, jcolg
    character(len=4) :: etam
    character(len=8) :: k8bid
    character(len=14) :: nonu
    character(len=16) :: k16bid, nomcmd
    character(len=19) :: nomat, nosolv
    character(len=24) :: kfiltr, kpiv, kpiv2, ksizemu
    real(kind=8) :: raux, rfiltr, epsmac, rmax, rmin, rtest
    complex(kind=8) :: caux
    logical :: lmnsy, ltypr, lnn, lfiltr, lspd, eli2lg, lsimpl, lcmde
!
!-----------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)
!
!       ------------------------------------------------
!        INITS
!       ------------------------------------------------
!
    epsmac=r8prem()
    nomat=nomats(kxmps)
    nosolv=nosols(kxmps)
    nonu=nonus(kxmps)
    etam=etams(kxmps)
! --- REMPLISSAGE DE DIFFERENTS OBJETS SUIVANT LE TYPE DU POINTEUR
! --- DE MUMPS: DMUMPS_STRUC OU ZMUMPS_STRUC
    if (type .eq. 'S') then
        smpsk=>smps(kxmps)
        ltypr=.true.
        sym=smpsk%sym
        rmax=r4maem()*0.5
        rmin=r4miem()*2.0
        nbproc=smpsk%nprocs
        rang=smpsk%myid
        esizemu=4
    else if (type.eq.'C') then
        cmpsk=>cmps(kxmps)
        ltypr=.false.
        sym=cmpsk%sym
        rmax=r4maem()*0.5
        rmin=r4miem()*2.0
        nbproc=cmpsk%nprocs
        rang=cmpsk%myid
        esizemu=8
    else if (type.eq.'D') then
        dmpsk=>dmps(kxmps)
        ltypr=.true.
        sym=dmpsk%sym
        rmax=r8maem()*0.5
        rmin=r8miem()*2.0
        nbproc=dmpsk%nprocs
        rang=dmpsk%myid
        esizemu=8
    else if (type.eq.'Z') then
        zmpsk=>zmps(kxmps)
        ltypr=.false.
        sym=zmpsk%sym
        rmax=r8maem()*0.5
        rmin=r8miem()*2.0
        nbproc=zmpsk%nprocs
        rang=zmpsk%myid
        esizemu=16
    else
        ASSERT(.false.)
    endif
!
    if (lmd) then
        if (lpreco) then
            call jeveuo(nonu//'.NUML.NLGP', 'L', jnulogl)
        else
            call jeveuo(nonu//'.NUML.NULG', 'L', jnulogl)
        endif
    else
        jnulogl=1
    endif
    if (ktypr(1:6) .eq. 'SYMDEF') then
        if ((type.eq.'C') .or. (type.eq.'Z')) then
            call utmess('F', 'FACTOR_80')
        endif
        lspd=.true.
    else
        lspd=.false.
    endif
!
    lsimpl = (type.eq.'S').or.(type.eq.'C')
!
!     ------------------------------------------
!     DETERMINATION DE LA SYMETRIE DE LA MATRICE
!     ------------------------------------------
    call jelira(nomat//'.VALM', 'NMAXOC', nvale)
! --- LMNSY EST INDEPENDANT DE XMPSK%SYM POUR POUVOIR TRAITER
! --- DES CAS SYMETRIQUES EN MUMPS NON SYMETRIQUE
    if (nvale .eq. 1) then
        lmnsy=.false.
    else if (nvale.eq.2) then
        lmnsy=.true.
    else
        ASSERT(.false.)
    endif
!
!
!       ------------------------------------------------
!        lecture d'adresses et de parametres preliminaires
!       ------------------------------------------------
    if (((rang.eq.0).and.(.not.ldist)) .or. (ldist)) then
        call jeveuo(nonu//'.SMOS.SMDI', 'L', jsmdi)
        call jelira(nonu//'.SMOS.SMDI', 'LONMAX', nsmdi)
        call jeveuo(nonu//'.SMOS.SMHC', 'L', jsmhc)
        call jelira(nonu//'.SMOS.SMHC', 'LONMAX', nsmhc)
        if (lmd) then
            call jeveuo(nonu//'.NUML.DELG', 'L', jdelg)
            call jelira(nonu//'.NUML.DELG', 'LONMAX', n1)
        else
            call jeveuo(nonu//'.NUME.DELG', 'L', jdelg)
            call jelira(nonu//'.NUME.DELG', 'LONMAX', n1)
        endif
        call jeveuo(nonu//'.NUME.NEQU', 'L', jnequ)
        nbeq=to_mumps_int(zi(jnequ))
        ASSERT(n1.eq.nsmdi)
! --- CALCUL DE N
        n=nsmdi
!
! --- GESTION ELIM_LAGR2
! --- on a essaye une basule automatique elim_lagr2='oui'/'non'
! --- en fonction de la proportion de lagranges. en fait, 'oui'
! --- OFFRE LA PLUPART DU TEMPS LE MEILLEUR COMPROMIS:
! ---     CPU x MEMOIRE x QUALITE --> ON NE PROGRAMME PAS DE
! --- bascule, on laisse 'oui' par defaut (pour l'instant)
        select case(klag2(1:3))
        case('OUI')
        eli2lg=.true.
        case('NON')
        eli2lg=.false.
        case default
        ASSERT(.false.)
        end select
!
! --- CALCUL DE NZ2
        nz=zi(jsmdi-1+n)
        ASSERT(nz.le.nsmhc)
        nz2=to_mumps_int(nz)
        if (sym .eq. 0) nz2=to_mumps_int(2*nz-n)
!
        call jeveuo(jexnum(nomat//'.VALM', 1), 'L', jvale)
        call jelira(jexnum(nomat//'.VALM', 1), 'LONMAX', nlong)
        ASSERT(nlong.eq.nz)
        if (lmnsy) then
            call jeveuo(jexnum(nomat//'.VALM', 2), 'L', jvale2)
            call jelira(jexnum(nomat//'.VALM', 2), 'LONMAX', nlong)
            ASSERT(nlong.eq.nz)
        endif
!
! ---  DETERMINATION DES TERMES DE REFERENCE POUR LE FILTRAGE
! ---  NFILT1 : TERMES RIGOUREUSEMENT NULS
! ---  NFILT2 : DEPASSEMENT DE CAPACITE PAR VALEUR MAX
! ---  NFILT3 : DEPASSEMENT DE CAPACITE PAR VALEUR MIN
        lfiltr=.false.
        kfiltr='&&AMUMPM.FILTRAGE'
        nfilt1=0
        nfilt2=0
        nfilt3=0
        if (epsmat .gt. 0.d0) then
            lfiltr=.true.
            call jeexin(kfiltr, iret)
            if (iret .ne. 0) call jedetr(kfiltr)
            call wkvect(kfiltr, 'V V R', n, ifiltr)
            if (ltypr) then
                do k = 1, n
                    zr(ifiltr-1+k)=epsmat*abs(zr(jvale-1+zi(jsmdi-1+k)))
                enddo
            else
                do k = 1, n
                    zr(ifiltr-1+k)=epsmat*abs(zc(jvale-1+zi(jsmdi-1+k)))
                enddo
            endif
! --- SEUILLAGE DES TERMES DE FILTRAGE POUR EVITER LES VALEURS ABBERANTE
            do k = 1, n
                rtest=zr(ifiltr-1+k)
                if (rtest .lt. rmin) zr(ifiltr-1+k)=0.d0
                if (rtest .gt. rmax) zr(ifiltr-1+k)=rmax
            enddo
        else
            lfiltr=.false.
        endif
    endif
!
!       ------------------------------------------------
!       DETERMINATION DU NBRE LOCAL DE TERMES PAR PROC: NZLOC
!       EN CENTRALISE (LDIST=.FALSE.): PROC 0 GERE TOUS LES TERMES POUR
!         LES FOURNIR A MUMPS. CE DERNIER ENSUITE LES REDISPATCHE PAR
!         PAQUETS SUR TOUS LES AUTRES PROCS.
!       EN DISTRIBUE (LDIST=.TRUE.): CHAQUE PROC FOURNIT LES TERMES
!         DONT IL A LA RESPONSABILITE.
!       ------------------------------------------------
!
    if (((rang.eq.0).and.(.not.ldist)) .or. (ldist)) then
!
! --- VECTEURS AUXILIAIRES POUR FILTRER LES TERMES MATRICIELS
! --- KPIV: PROFIL TRIANGULAIRE INF
! --- KPIV2: IDEM SUP
        kpiv='&&AMUMPM.TERMEOK'
        kpiv2='&&AMUMPM.TERMEOK2'
        call jeexin(kpiv, ibid)
        if (ibid .ne. 0) then
            ASSERT(.false.)
        else
            call wkvect(kpiv, 'V V S', nz, iok)
            do k = 1, nz
                zi4(iok+k-1)=0
            enddo
        endif
        if (sym .eq. 0) then
            call jeexin(kpiv2, ibid)
            if (ibid .ne. 0) then
                ASSERT(.false.)
            else
                call wkvect(kpiv2, 'V V S', nz, iok2)
                do k = 1, nz
                    zi4(iok2+k-1)=0
                enddo
            endif
        endif
!
        nzloc=0
        jcoll=1
        do kterm = 1, nz
!
! --- PREPARATION DES DONNES (NUM DE COLONNE, TERME DE FILTRAGE...)
            if (zi(jsmdi-1+jcoll) .lt. kterm) jcoll=jcoll+1
            iligl=zi4(jsmhc-1+kterm)
            if (lfiltr) then
                rfiltr=zr(ifiltr-1+iligl)+zr(ifiltr-1+jcoll)
            else
                rfiltr=-1.d0
            endif
!
!
! --- PARTIE TRIANGULAIRE INF. SI REEL
            if (ltypr) then
                raux=zr(jvale-1+kterm)
                if (raux .ne. 0.d0) then
                    rtest=abs(raux)
                    if (rtest .gt. rfiltr) then
                        if (rtest .lt. rmin) then
                            nfilt3=nfilt3+1
                            zi4(iok+kterm-1)=-2
                        else if (rtest.gt.rmax) then
                            nfilt2=nfilt2+1
                            zi4(iok+kterm-1)=-1
                        else
                            zi4(iok+kterm-1)=1
                        endif
                        nzloc=nzloc+1
                    endif
                else
! ---   TERME RIGOUREUSEMENT NUL
                    nfilt1=nfilt1+1
                endif
!
            else
!
! --- PARTIE TRIANGULAIRE INF. SI COMPLEXE
                caux=zc(jvale-1+kterm)
                if (caux .ne. (0.d0,0.d0)) then
                    rtest=abs(caux)
                    if (rtest .gt. rfiltr) then
                        if (rtest .lt. rmin) then
                            nfilt3=nfilt3+1
                            zi4(iok+kterm-1)=-2
                        else if (rtest.gt.rmax) then
                            nfilt2=nfilt2+1
                            zi4(iok+kterm-1)=-1
                        else
                            zi4(iok+kterm-1)=1
                        endif
                        nzloc=nzloc+1
                    endif
                else
! ---   TERME RIGOUREUSEMENT NUL
                    nfilt1=nfilt1+1
                endif
            endif
!
! --- PARTIE TRIANGULAIRE SUP. SI REEL
            if ((sym.eq.0) .and. (iligl.ne.jcoll)) then
!
                if (ltypr) then
                    if (lmnsy) raux=zr(jvale2-1+kterm)
                    if (raux .ne. 0.d0) then
                        rtest=abs(raux)
                        if (rtest .gt. rfiltr) then
                            if (rtest .lt. rmin) then
                                nfilt3=nfilt3+1
                                zi4(iok2+kterm-1)=-2
                            else if (rtest.gt.rmax) then
                                nfilt2=nfilt2+1
                                zi4(iok2+kterm-1)=-1
                            else
                                zi4(iok2+kterm-1)=1
                            endif
                            nzloc=nzloc+1
                        endif
                    else
! ---   TERME RIGOUREUSEMENT NUL
                        nfilt1=nfilt1+1
                    endif
!
                else
!
! --- PARTIE TRIANGULAIRE SUP. SI COMPLEXE
                    if (lmnsy) caux=zc(jvale2-1+kterm)
                    if (caux .ne. (0.d0,0.d0)) then
                        rtest=abs(caux)
                        if (rtest .gt. rfiltr) then
                            if (rtest .lt. rmin) then
                                nfilt3=nfilt3+1
                                zi4(iok2+kterm-1)=-2
                            else if (rtest.gt.rmax) then
                                nfilt2=nfilt2+1
                                zi4(iok2+kterm-1)=-1
                            else
                                zi4(iok2+kterm-1)=1
                            endif
                            nzloc=nzloc+1
                        endif
                    else
! ---   TERME RIGOUREUSEMENT NUL
                        nfilt1=nfilt1+1
                    endif
!
                endif
            endif
        enddo
        nz2=to_mumps_int(nzloc)
        if (niv .ge. 2) then
            write(ifm,*)'<AMUMPM>     NZLOC: ',nzloc
            write(ifm,*)'       TERMES NULS: ',nfilt1
            write(ifm,*)'   UNDER/OVERFLOWS: ',nfilt3,'/',nfilt2
        endif
    endif
!
!       ------------------------------------------------
!       ALLOCATION DES OBJETS MUMPS F90
!       ------------------------------------------------
!
! ---   OBJET JEVEUX STOCKANT, PAR PROC, LA TAILLE DES OBJETS ALLOUES
! ---   POUR MUMPS. UTILE A AMUMPU. ON SUPPOSE UN SEUL RHS.
! ---   EN FIN DE ROUTINE, TOUS LES PROCS CONNAISSENT CE VECTEUR
! ----  VIA UN MPI_ALLREDUCE + SUM.
    ksizemu='&&TAILLE_OBJ_MUMPS'
    call jeexin(ksizemu, iret)
    if (iret .eq. 0) then
        call wkvect(ksizemu, 'V V I', nbproc, isizemu)
    else
        call jeveuo(ksizemu, 'E', isizemu)
    endif
    do k = 1, nbproc
        zi(isizemu+k-1)=0
    enddo
    nsizemu=0
    if ((( rang.eq.0).and.(.not.ldist)) .or. (ldist)) then
        if (ldist) then
            if (type .eq. 'S') then
                if (lmd) then
                    smpsk%n=nbeq
                else
                    smpsk%n=to_mumps_int(n)
                endif
                smpsk%nz_loc=nz2
                allocate(smpsk%irn_loc(nz2))
                allocate(smpsk%jcn_loc(nz2))
                allocate(smpsk%a_loc(nz2))
            else if (type.eq.'C') then
                cmpsk%n=to_mumps_int(n)
                cmpsk%nz_loc=nz2
                allocate(cmpsk%irn_loc(nz2))
                allocate(cmpsk%jcn_loc(nz2))
                allocate(cmpsk%a_loc(nz2))
            else if (type.eq.'D') then
                if (lmd) then
                    dmpsk%n=nbeq
                else
                    dmpsk%n=to_mumps_int(n)
                endif
                dmpsk%nz_loc=nz2
                allocate(dmpsk%irn_loc(nz2))
                allocate(dmpsk%jcn_loc(nz2))
                allocate(dmpsk%a_loc(nz2))
            else if (type.eq.'Z') then
                zmpsk%n=to_mumps_int(n)
                zmpsk%nz_loc=nz2
                allocate(zmpsk%irn_loc(nz2))
                allocate(zmpsk%jcn_loc(nz2))
                allocate(zmpsk%a_loc(nz2))
            else
                ASSERT(.false.)
            endif
            if (lmd) then
                nsizemu=nz2*(4+4+esizemu)+esizemu*nbeq
            else
                nsizemu=nz2*(4+4+esizemu)+esizemu*n
            endif
        else
            if (type .eq. 'S') then
                smpsk%n=to_mumps_int(n)
                smpsk%nz=nz2
                allocate(smpsk%irn(nz2))
                allocate(smpsk%jcn(nz2))
                allocate(smpsk%a(nz2))
            else if (type.eq.'C') then
                cmpsk%n=to_mumps_int(n)
                cmpsk%nz=nz2
                allocate(cmpsk%irn(nz2))
                allocate(cmpsk%jcn(nz2))
                allocate(cmpsk%a(nz2))
            else if (type.eq.'D') then
                dmpsk%n=to_mumps_int(n)
                dmpsk%nz=nz2
                allocate(dmpsk%irn(nz2))
                allocate(dmpsk%jcn(nz2))
                allocate(dmpsk%a(nz2))
            else if (type.eq.'Z') then
                zmpsk%n=to_mumps_int(n)
                zmpsk%nz=nz2
                allocate(zmpsk%irn(nz2))
                allocate(zmpsk%jcn(nz2))
                allocate(zmpsk%a(nz2))
            else
                ASSERT(.false.)
            endif
            nsizemu=nz2*(4+4+esizemu)+esizemu*n
        endif
        zi(isizemu+rang)=1+(nsizemu/(1024*1024))
!
!       ------------------------------------------------
!       INTERPRETATION DES PBS RENCONTRES LORS DU FILTRAGE
!       REMPLISSAGE DE LA MATRICE
!       EN CAS D'OVERFLOW:
!         SI SOLVEUR DIRECT: UTMESS_F
!         SI SIMPLE PRECISION : ON SEUILLE LES TERMES IMPACTES
!                               LORS DU REMPLISSAGE EFFECTIF.
!       EN CAS D'UNDERFLOW: ON FIXE A ZERO.
!       ------------------------------------------------
!
        if (.not.lsimpl) then
            vali(1)=nfilt3
            vali(2)=nfilt2
            do kterm = 1, nz
                if (zi4(iok+kterm-1) .eq. -1) then
                    call utmess('F', 'FACTOR_78', ni=2, vali=vali)
                endif
            enddo
            if (sym .eq. 0) then
                do kterm = 1, nz
                    if (zi4(iok2+kterm-1) .eq. -1) then
                        call utmess('F', 'FACTOR_78', ni=2, vali=vali)
                    endif
                enddo
            endif
        endif
!
! --- REMPLISSAGE EFFECTIF DES TERMES DE LA MATRICE
        jcoll=1
        iterm=0
        do kterm = 1, nz
!
            if (zi(jsmdi-1+jcoll) .lt. kterm) jcoll=jcoll+1
            iligl=zi4(jsmhc-1+kterm)
            lnn=.false.
! --- PARTIE TRIANGULAIRE INF. SI REEL
            if (ltypr) then
                if (zi4(iok+kterm-1) .eq. 1) then
                    lnn=.true.
                    raux=zr(jvale-1+kterm)
                else if (zi4(iok+kterm-1).eq.-1) then
                    lnn=.true.
                    raux=zr(jvale-1+kterm)
                    raux=rmax*sign(1.d0,raux)
                endif
            else
! --- PARTIE TRIANGULAIRE INF. SI COMPLEXE
                if (zi4(iok+kterm-1) .eq. 1) then
                    lnn=.true.
                    caux=zc(jvale-1+kterm)
                else if (zi4(iok+kterm-1).eq.-1) then
                    lnn=.true.
                    caux=zc(jvale-1+kterm)
                    caux=rmax*dcmplx(1.d0*sign(1.d0,dble(caux)), 1.d0*&
                sign(1.d0,imag(caux)))
                endif
            endif
            if (lmd) then
                iligg=to_mumps_int(zi(jnulogl+iligl-1))
                jcolg=to_mumps_int(zi(jnulogl+jcoll-1))
            else
                iligg=to_mumps_int(iligl)
                jcolg=to_mumps_int(jcoll)
            endif
            if ((sym.ne.0) .and. (iligg.ge.jcolg)) then
                coltmp=jcolg
                jcolg=to_mumps_int(iligg)
                iligg=to_mumps_int(coltmp)
            endif
!
! ---- PARTIE TRIANGULAIRE INF. TERME NON NUL
            if (lnn) then
                iterm=iterm+1
                if (ldist) then
                    if (type .eq. 'S') then
                        smpsk%irn_loc(iterm)=iligg
                        smpsk%jcn_loc(iterm)=jcolg
                        smpsk%a_loc(iterm)=real(raux, kind=4)
                    else if (type.eq.'C') then
                        cmpsk%irn_loc(iterm)=iligg
                        cmpsk%jcn_loc(iterm)=jcolg
                        cmpsk%a_loc(iterm)=cmplx(caux, kind=4)
                    else if (type.eq.'D') then
                        dmpsk%irn_loc(iterm)=iligg
                        dmpsk%jcn_loc(iterm)=jcolg
                        dmpsk%a_loc(iterm)=raux
                    else if (type.eq.'Z') then
                        zmpsk%irn_loc(iterm)=iligg
                        zmpsk%jcn_loc(iterm)=jcolg
                        zmpsk%a_loc(iterm)=caux
                    else
                        ASSERT(.false.)
                    endif
                else
                    if (type .eq. 'S') then
                        smpsk%irn(iterm)=iligg
                        smpsk%jcn(iterm)=jcolg
                        smpsk%a(iterm)=real(raux, kind=4)
                    else if (type.eq.'C') then
                        cmpsk%irn(iterm)=iligg
                        cmpsk%jcn(iterm)=jcolg
                        cmpsk%a(iterm)=cmplx(caux, kind=4)
                    else if (type.eq.'D') then
                        dmpsk%irn(iterm)=iligg
                        dmpsk%jcn(iterm)=jcolg
                        dmpsk%a(iterm)=raux
                    else if (type.eq.'Z') then
                        zmpsk%irn(iterm)=iligg
                        zmpsk%jcn(iterm)=jcolg
                        zmpsk%a(iterm)=caux
                    else
                        ASSERT(.false.)
                    endif
                endif
                kzero=0
                if (eli2lg) then
! ------      ON ELIMINE LE DERNIER TERME DE A/A_loc SI LAG2
                    if (zi(jdelg-1+iligl) .eq. -1) then
                        if (jcoll .eq. iligl) kzero=1
                        if (zi(jdelg-1+jcoll) .eq. -2) kzero=1
                    endif
                    if (zi(jdelg-1+iligl) .eq. -2) then
                        if (jcoll .ne. iligl) kzero=1
                    endif
                    if (zi(jdelg-1+jcoll) .eq. -2) then
                        if (jcoll .ne. iligl) kzero=1
                    endif
                    if (kzero .eq. 1) iterm=iterm-1
                endif
! --- SI RESOLUTION SPD DEMANDEE ET TERME NEGATIF OU NUL ON S'ARRETE
                if ((lspd) .and. (kzero.eq.0)) then
                    if ((iligg.eq.jcolg) .and. (raux.lt.epsmac)) then
                        call utmess('F', 'FACTOR_84')
                    endif
                endif
            endif
!
! --- PARTIE TRIANGULAIRE SUP. SI REEL
            if ((sym.eq.0) .and. (iligl.ne.jcoll)) then
!
                lnn=.false.
                if (ltypr) then
                    if (zi4(iok2+kterm-1) .eq. 1) then
                        lnn=.true.
                        if (lmnsy) raux=zr(jvale2-1+kterm)
                    else if (zi4(iok2+kterm-1).eq.-1) then
                        lnn=.true.
                        raux=zr(jvale-1+kterm)
                        raux=rmax*sign(1.d0,raux)
                    endif
                else
! --- PARTIE TRIANGULAIRE SUP. SI COMPLEXE
                    if (zi4(iok2+kterm-1) .eq. 1) then
                        lnn=.true.
                        if (lmnsy) caux=zc(jvale2-1+kterm)
                    else if (zi4(iok2+kterm-1).eq.-1) then
                        lnn=.true.
                        caux=zc(jvale-1+kterm)
                        caux=rmax*dcmplx(1.d0*sign(1.d0,dble(caux)),&
                    1.d0*sign(1.d0,imag(caux)))
                    endif
                endif
!
! ---- PARTIE TRIANGULAIRE SUP. TERME NON NUL
                if (lnn) then
                    iterm=iterm+1
                    if (lmd) then
                        iligg=to_mumps_int(zi(jnulogl+iligl-1))
                        jcolg=to_mumps_int(zi(jnulogl+jcoll-1))
                    else
                        iligg=to_mumps_int(iligl)
                        jcolg=to_mumps_int(jcoll)
                    endif
                    if (ldist) then
                        if (type .eq. 'S') then
                            smpsk%irn_loc(iterm)=jcolg
                            smpsk%jcn_loc(iterm)=iligg
                            smpsk%a_loc(iterm)=real(raux, kind=4)
                        else if (type.eq.'C') then
                            cmpsk%irn_loc(iterm)=jcolg
                            cmpsk%jcn_loc(iterm)=iligg
                            cmpsk%a_loc(iterm)=cmplx(caux, kind=4)
                        else if (type.eq.'D') then
                            dmpsk%irn_loc(iterm)=jcolg
                            dmpsk%jcn_loc(iterm)=iligg
                            dmpsk%a_loc(iterm)=raux
                        else if (type.eq.'Z') then
                            zmpsk%irn_loc(iterm)=jcolg
                            zmpsk%jcn_loc(iterm)=iligg
                            zmpsk%a_loc(iterm)=caux
                        else
                            ASSERT(.false.)
                        endif
                    else
                        if (type .eq. 'S') then
                            smpsk%irn(iterm)=jcolg
                            smpsk%jcn(iterm)=iligg
                            smpsk%a(iterm)=real(raux, kind=4)
                        else if (type.eq.'C') then
                            cmpsk%irn(iterm)=jcolg
                            cmpsk%jcn(iterm)=iligg
                            cmpsk%a(iterm)=cmplx(caux, kind=4)
                        else if (type.eq.'D') then
                            dmpsk%irn(iterm)=jcolg
                            dmpsk%jcn(iterm)=iligg
                            dmpsk%a(iterm)=raux
                        else if (type.eq.'Z') then
                            zmpsk%irn(iterm)=jcolg
                            zmpsk%jcn(iterm)=iligg
                            zmpsk%a(iterm)=caux
                        else
                            ASSERT(.false.)
                        endif
                    endif
                    if (eli2lg) then
                        if (kzero .eq. 1) iterm=iterm-1
                    endif
! --- SI RESOLUTION SPD DEMANDEE ET TERME NEGATIF OU NUL ON S'ARRETE
                    if ((lspd) .and. (kzero.eq.0)) then
                        if ((iligg.eq.jcolg) .and. (raux.lt.epsmac)) then
                            call utmess('F', 'FACTOR_84')
                        endif
                    endif
                endif
            endif
! ---FIN DE LA BOUCLE SUR NZ
        enddo
!
        ASSERT(iterm.le.nz2)
        nz2=to_mumps_int(iterm)
        if (ldist) then
            if (type .eq. 'S') then
                smpsk%nz_loc=nz2
            else if (type.eq.'C') then
                cmpsk%nz_loc=nz2
            else if (type.eq.'D') then
                dmpsk%nz_loc=nz2
            else if (type.eq.'Z') then
                zmpsk%nz_loc=nz2
            else
                ASSERT(.false.)
            endif
        else
            if (type .eq. 'S') then
                smpsk%nz=nz2
            else if (type.eq.'C') then
                cmpsk%nz=nz2
            else if (type.eq.'D') then
                dmpsk%nz=nz2
            else if (type.eq.'Z') then
                zmpsk%nz=nz2
            else
                ASSERT(.false.)
            endif
        endif
        ASSERT(iligl.eq.n)
        ASSERT(jcoll.eq.n)
        call jelibe(nonu//'.SMOS.SMDI')
        call jelibe(nonu//'.SMOS.SMHC')
        call jelibe(jexnum(nomat//'.VALM', 1))
        if (lmnsy) call jelibe(jexnum(nomat//'.VALM', 2))
        call jelibe(nonu//'.NUME.DELG')
        if (lmd) then
            call jelibe(nonu//'.NUML.DELG')
        endif
!
        if (niv .ge. 2) then
! --- TEST POUR EVITER LE MONITORING DES CMDES ECLATEES ET DE LDLT_SP
!     LES OBJETS TEMPORAIRES DE MONITORING SONT EFFACES A CHAQUE
!     FIN DE COMMANDE (NUM_DDL/FACTORISER/RESOUDRE)
            call getres(k8bid, k16bid, nomcmd)
            if ((nomcmd(1:8).eq.'NUME_DDL') .or. (nomcmd(1:10).eq.'FACTORISER') .or.&
                (nomcmd(1:8).eq.'RESOUDRE')) then
                lcmde=.true.
            else
                lcmde=.false.
            endif
            if ((.not.lcmde) .and. (.not.lpreco)) then
                call jeexin(kmonit(1), iret)
                if (iret .ne. 0) then
! --- CAS CMDE STD AVEC MUMPS SOLVEUR DIRECT
                    call jeveuo(kmonit(1), 'E', ibid)
                    zi(ibid+rang)=nz2
                else
! --- L'OBJET KMONIT(1) DEVRAIT EXISTER
                    ASSERT(.false.)
                endif
            endif
        endif
!
!
!       ------------------------------------------------
!       IMPRESSION DE LA MATRICE (SI DEMANDEE) :
!       ------------------------------------------------
        if (impr(1:3) .eq. 'OUI') then
            write(ifmump,*)sym,'   : SYM', rang,'   : RANG'
            write(ifmump,*)n,'   : N'
            if (ldist) then
                write(ifmump,*)nz2,'   : NZ_loc'
            else
                write(ifmump,*)nz2,'   : NZ'
            endif
            if (type .eq. 'S') then
                do k = 1, nz2
                    if (ldist) then
                        write(ifmump,*)smpsk%irn_loc(k),smpsk%jcn_loc(k),&
                    smpsk%a_loc(k)
                    else
                        write(ifmump,*)smpsk%irn(k),smpsk%jcn(k),smpsk%a(&
                    k)
                    endif
                enddo
            else if (type.eq.'C') then
                do k = 1, nz2
                    if (ldist) then
                        write(ifmump,*)cmpsk%irn_loc(k),cmpsk%jcn_loc(k),&
                    cmpsk%a_loc(k)
                    else
                        write(ifmump,*)cmpsk%irn(k),cmpsk%jcn(k),cmpsk%a(&
                    k)
                    endif
                enddo
            else if (type.eq.'D') then
                do k = 1, nz2
                    if (ldist) then
                        write(ifmump,*)dmpsk%irn_loc(k),dmpsk%jcn_loc(k),&
                    dmpsk%a_loc(k)
                    else
                        write(ifmump,*)dmpsk%irn(k),dmpsk%jcn(k),dmpsk%a(&
                    k)
                    endif
                enddo
            else if (type.eq.'Z') then
                do k = 1, nz2
                    if (ldist) then
                        write(ifmump,*)zmpsk%irn_loc(k),zmpsk%jcn_loc(k),&
                    zmpsk%a_loc(k)
                    else
                        write(ifmump,*)zmpsk%irn(k),zmpsk%jcn(k),zmpsk%a(&
                    k)
                    endif
                enddo
            else
                ASSERT(.false.)
            endif
            if (ldist) then
                write(ifmump,*) 'MUMPS FIN A_loc'
            else
                write(ifmump,*) 'MUMPS FIN A'
            endif
        endif
! FIN DU IF LDIST
    endif
!
! --- COMMUNICATION DU VECTEUR KSIZEMU A TOUS LES PROCS
    call asmpi_comm_jev('MPI_SUM', ksizemu)
!
! --- NETTOYAGE VECTEURS TEMPORAIRES LOCAUX
    if (((rang.eq.0).and.(.not.ldist)) .or. (ldist)) then
        call jeexin(kfiltr, iret)
        if (iret .ne. 0) call jedetr(kfiltr)
        call jedetr(kpiv)
        if (sym .eq. 0) call jedetr(kpiv2)
    endif
!
! --- DECHARGEMENT CIBLE D'OBJETS JEVEUX
    call jelibd(nonu//'.SMOS.SMDI', ltot)
    call jelibd(nonu//'.SMOS.SMHC', ltot)
    call jelibd(nonu//'.NUME.DEEQ', ltot)
    call jelibd(nonu//'.NUME.NUEQ', ltot)
    call jelibd(nonu//'.NUME.LILI', ltot)
    call jelibd(jexnum(nomat//'.VALM', 1), ltot)
    if (lmnsy) call jelibd(jexnum(nomat//'.VALM', 2), ltot)
    if (lmd) then
        call jelibd(nonu//'.NUML.NULG', ltot)
        call jelibd(nonu//'.NUML.DELG', ltot)
    else
        call jelibd(nonu//'.NUME.DELG', ltot)
    endif
!
    call jedema()
#endif
end subroutine
