subroutine vpmpi(option, eigsol,&
                 icom1, icom2, lcomod, mpicou, mpicow, nbvecg, nfreqg, rangl, omemax, omemin,&
                 vpinf, vpmax)

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
! -------------------------------------------------------------------------------------------------
! ROUTINE ORGANISANT LE PARALLELISME MULTI-NIVEAUX DANS MODE_ITER_SIMULT (+ APPELS DS VPPARA).
! -------------------------------------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
    implicit none

#include "asterf_types.h"
#include "asterc/asmpi_comm.h"
#include "asterc/asmpi_split_comm.h"
#include "asterfort/asmpi_barrier.h"
#include "asterfort/asmpi_comm_vect.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/utmess.h"
#include "asterfort/vpleci.h"

!
! --- INPUT
!
    integer           , intent(in) :: option
    character(len=19) , intent(in) :: eigsol
!
! --- OUTPUT
!
    integer           , intent(out) :: icom1, icom2, nbvecg, nfreqg
!
! --- INPUT/OUTPUT
!
    mpi_int           , intent(inout) :: mpicou, mpicow
    integer           , intent(inout) :: rangl
    aster_logical     , intent(inout) :: lcomod
    real(kind=8)      , intent(inout) :: omemax, omemin, vpinf, vpmax
!
! --- VARIABLES LOCALES
!
    mpi_int           :: mrang, mnbproc
    integer           :: l1, l2, l3, nbvect, nbproc, nfreq, rang, typeco, vali(5)
    real(kind=8)      :: rbid
    character(len=24) :: k24bid, valk(5)
!
! -----------------------
! --- CORPS DE LA ROUTINE
! -----------------------


    select case(option)
    case(1)
! ---  STEP 1: RECUPERATION DES PARAMETRES MPI + TESTS
! --- INPUT: option
! --- OUTPUT: mpicou, mpicow, icom1, icom2, rangl, lcomod
        icom1=-999
        icom2=-999
        call asmpi_comm('GET_WORLD', mpicow)
        call asmpi_comm('GET', mpicou)
! --  ON EST CENSE FONCTIONNER EN COMM_WORLD
        if (mpicow .ne. mpicou) ASSERT(.false.)
        call asmpi_info(mpicow, mrang, mnbproc)
        rang = to_aster_int(mrang)
        nbproc = to_aster_int(mnbproc)

        call getvis('PARALLELISME_MACRO', 'TYPE_COM',   iocc=1, scal=typeco, nbret=l1)
        call getvis('PARALLELISME_MACRO', 'IPARA1_COM', iocc=1, scal=icom1,  nbret=l2)
        call getvis('PARALLELISME_MACRO', 'IPARA2_COM', iocc=1, scal=icom2,  nbret=l3)
        valk(1)='TYPE_COM'
        valk(2)='IPARA1_COM'
        valk(3)='IPARA2_COM'
        valk(4)='RANG'
        valk(5)='NBPROC'
        vali(1)=typeco
        vali(2)=icom1
        vali(3)=icom2
        vali(4)=rang
        vali(5)=nbproc
        if (l1*l2*l3.ne.1) call utmess('F', 'APPELMPI_6', nk=3, valk=valk, ni=3, vali=vali)

        if ((&
            ((typeco.ne.1).and.(typeco.ne.-999)) .or.&
            ((icom1.ne.-999).and.((icom1.lt.1).or.(icom1.gt.nbproc))) .or.&
            ((icom2.ne.-999).and.((icom2.lt.1).or.(icom2.gt.nbproc))) .or. (icom1.gt.icom2)&
            .or. (nbproc.lt.1) .or. (rang.lt.0)&
            )) call utmess('F', 'APPELMPI_8', nk=5, valk=valk, ni=5, vali=vali)


        if ((typeco.eq.1).and.(nbproc.gt.1)) then
            lcomod=.true.
! --  DECOMPOSE LE COM GLOBAL MPICOW EN COM LOCAL MPICOU
! --  PLUS AFFECTATION DE CE NOUVEAU COM AFIN DE NE PAS PERTURBER LA FACTO DE LA DEMI-BANDE
            call asmpi_split_comm(mpicow, to_mpi_int(icom1), to_mpi_int(0), 'ipara1', mpicou)
            if (mpicow .eq. mpicou) ASSERT(.false.)
            call asmpi_barrier()
            call asmpi_comm('SET', mpicou)
! --  RANG DANS LE SOUS-COMM MPICOU LIE A CHAQUE OCCURENCE MUMPS: RANGL
            call asmpi_info(comm=mpicou, rank=mrang)
            rangl = to_aster_int(mrang)
        else
            rangl=-9999
            mpicou=-9999
            lcomod=.false.
        endif

    case(2)
! --- STEP 2: REDIMENSIONNEMENT DES BUFFERS DE COMMUNICATION
! --- INPUT: option, eigsol, lcomod, mpicou, mpicow, rangl
! --- OUTPUT: nbvecg, nfreqg
        nbvecg=-9999
        nfreqg=-9999
        if (lcomod) then
            call vpleci(eigsol, 'I', 1, k24bid, rbid, nfreq)
            call vpleci(eigsol, 'I', 2, k24bid, rbid, nbvect)
! --  ON REMET LE COM WORLD POUR COMMUNIQUER NBVECT/NBFREQ
            call asmpi_comm('SET', mpicow)
            call asmpi_barrier()
! --  EST-ON LE PROCESSUS MAITRE DU COM LOCAL: RANGL=0 ?
! --  SI OUI, ON ENVOI LES BONNES VALEURS DE NBVECT/NFREQ SUR LE COM GLOBAL MPICOW,
! --  SINON ON RENVOI ZERO POUR NE PAS COMPTER PLUSIEURS FOIS L'INFO.
            if (rangl .eq. 0) then
                nbvecg=nbvect
                nfreqg=nfreq
            else
                nbvecg=0
                nfreqg=0
            endif
            call asmpi_comm_vect('MPI_SUM', 'I', sci=nbvecg)
            call asmpi_comm_vect('MPI_SUM', 'I', sci=nfreqg)
! --  ON REMET LE COM LOCAL POUR LES FACTO ET SOLVES A SUIVRE
            call asmpi_barrier()
            call asmpi_comm('SET', mpicou)
        endif

    case(3)
! --- STEP 3: POUR MEMOIRE, ETAPE GEREE DANS VPPARA
      ASSERT(.false.)

    case(4)
! --- STEP 4:
! --- EN CAS DE TEST DE STURM LOCAL A CHAQUE SOUS-BANDE, REMISE A JOUR DES BORNES VIA LE COM WORLD.
! --- PUIS ON REMET LE COMCOU POUR NE PAS GENER LES FACTOS EVENTUELLES DE VPCNTL.
! --- INPUT: lcomod, mpicow, mpicou
! --- INPUT/OUTPUT: omemin, omemax, vpinf, vpmax
        if (lcomod) then
            call asmpi_comm('SET', mpicow)
            call asmpi_barrier()
            call asmpi_comm_vect('MPI_MIN', 'R', scr=omemin)
            call asmpi_comm_vect('MPI_MIN', 'R', scr=vpinf)
            call asmpi_comm_vect('MPI_MAX', 'R', scr=omemax)
            call asmpi_comm_vect('MPI_MAX', 'R', scr=vpmax)
            call asmpi_barrier()
            call asmpi_comm('SET', mpicou)
        endif

    case(5)
! --- STEP 5:
! --- AVANT DE QUITTER L'OP. ON REMET LE COM WORLD (AU CAS OU).
! --- DESTRUCTION DES SOUS-COMMUNICATEURS EVENTUELLEMENT ASSOCIES A UNE OCCURENCE MUMPS
! --- UNE OCCURENCE MUMPS (APRES CELLE DE LADITE OCCURENCE).
! --- INPUT: lcomod, mpicow, mpicou
        if (lcomod) then
            call asmpi_comm('SET', mpicow)
            call asmpi_barrier()
            call asmpi_comm('FREE', mpicou)
        endif

    case default
        ASSERT(.false.)
    end select
!
!     FIN DE VPMPI
!
end subroutine
