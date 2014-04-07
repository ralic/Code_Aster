subroutine uttcpi(nommes, ifm, typimp)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/asmpi_comm_vect.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/stati1.h"
!
    character(len=*) :: nommes, typimp
    integer :: ifm
! ----------------------------------------------------------------------
! BUT : IMPRIMER UNE MESURE DE TEMPS
!
! IN  NOMMES    : NOM IDENTIFIANT LA MESURE
!
! IN  IFM       : UNITE LOGIQUE POUR LES IMPRESSIONS
! IN  TYPIMP = 'CUMU' : ON IMPRIME LE "CUMUL" DE LA MESURE
!            = 'INCR' : ON IMPRIME L'INCREMENT DE LA MESURE
!
! ON APPELLE "INCREMENT" LA DIFFERENCE DE TEMPS ENTRE 2 APPELS
! SUCCESSIFS à UTTCPI(NOMMES,*,'INCR')
! ----------------------------------------------------------------------
! REMARQUE : LES VALEURS STOCKEES SONT ACCUMUEES VIA UTTCPU
    logical :: ljev
    integer :: indi, jvalms, k,  nbfois, jvalmi
    integer :: rang, nbproc, npromx, i1
    parameter (npromx=1000)
    character(len=8) :: numes
    character(len=50) :: nommel
    real(kind=8) :: xtota, xuser, xsyst, xelap, moyenn(3), ectype(3)
    real(kind=8) :: xtotap(npromx), xsystp(npromx), xelapp(npromx)
    logical :: lmesur
!
!
!     -- COMMONS POUR MESURE DE TEMPS :
    integer :: mtpniv, mtpsta, indmax
    parameter (indmax=5)
    character(len=80) :: snolon(indmax)
    real(kind=8) :: valmes(indmax*7), valmei(indmax*7)
    character(len=80), pointer :: nomlon(:) => null()
    mpi_int :: mrank, msize
    common /mestp1/ mtpniv,mtpsta
    common /mestp2/ snolon
    common /mestp3/ valmes,valmei
!
! ----------------------------------------------------------------------
!
!     1. CALCUL DE INDI ET LJEV :
!     -------------------------------
!     -- POUR CERTAINES MESURES, ON NE PEUT PAS FAIRE DE JEVEUX :
!        ON GARDE ALORS LES INFOS DANS LES COMMON MESTPX
    if (nommes .eq. 'CPU.MEMD.1') then
        indi=1
    else if (nommes.eq.'CPU.MEMD.2') then
        indi=2
    else
        ljev=.true.
        call jenonu(jexnom('&&UTTCPU.NOMMES', nommes), indi)
        if (indi .eq. 0) goto 9999
        goto 9998
    endif
    ASSERT(indi.le.indmax)
    ljev=.false.
!
9998  continue
!
    if (ljev) then
        call jeveuo('&&UTTCPU.VALMES', 'L', jvalms)
        nbfois = nint(zr(jvalms-1+7*(indi-1)+2))
    else
        nbfois = nint(valmes(7*(indi-1)+2))
    endif
!
    lmesur=.true.
    if (nbfois .eq. 0) then
        lmesur=.false.
    endif
!
    if (lmesur) then
        if (ljev) then
            xuser = zr(jvalms-1+7*(indi-1)+5)
            xsyst = zr(jvalms-1+7*(indi-1)+6)
            xelap = zr(jvalms-1+7*(indi-1)+7)
        else
            xuser = valmes(7*(indi-1)+5)
            xsyst = valmes(7*(indi-1)+6)
            xelap = valmes(7*(indi-1)+7)
        endif
    else
        xuser=0.d0
        xsyst=0.d0
        xelap=0.d0
    endif
!
    if (typimp .eq. 'CUMU') then
!
    else if (typimp.eq.'INCR') then
        if (lmesur) then
            if (ljev) then
                call jeveuo('&&UTTCPU.VALMEI', 'E', jvalmi)
                xuser = xuser - zr(jvalmi-1+7*(indi-1)+5)
                xsyst = xsyst - zr(jvalmi-1+7*(indi-1)+6)
                xelap = xelap - zr(jvalmi-1+7*(indi-1)+7)
!
                do 2,k=1,7
                zr(jvalmi-1+7*(indi-1)+k)=zr(jvalms-1+7*(indi-1)+&
                    k)
 2              continue
            else
                call jeveuo('&&UTTCPU.VALMEI', 'E', jvalmi)
                xuser = xuser - valmei(7*(indi-1)+5)
                xsyst = xsyst - valmei(7*(indi-1)+6)
                xelap = xelap - valmei(7*(indi-1)+7)
!
                do 3,k=1,7
                valmei(7*(indi-1)+k)=valmes(7*(indi-1)+k)
 3              continue
            endif
        else
            xuser=0.d0
            xsyst=0.d0
            xelap=0.d0
        endif
!
    else
        ASSERT(.false.)
    endif
!
!     -- NOMMEL, NUMES :
    if (ljev) then
        call jeveuo('&&UTTCPU.NOMLON', 'L', vk80=nomlon)
        nommel=nomlon(indi)(1:50)
    else
        nommel=snolon(indi)(1:50)
    endif
    i1= index(nommel,'#')
    ASSERT(i1.gt.1 .and. i1.le.8)
    numes='#'//nommel(1:i1-1)
    nommel=nommel(i1+1:)
!
!
!     -- EN SEQUENTIEL, ON IMPRIME LES 3 NOMBRES MESURES
!     ------------------------------------------------------
    xtota=xuser+xsyst
    if (lmesur) then
        write(ifm,1003) numes,nommel,xtota,xsyst,xelap
    endif
!
!     -- EN PARALLELE, ON IMPRIME EN PLUS LA MOYENNE ET
!        L'ECART TYPE (SUR L'ENSEMBLE DES PROCS)
!        SI MTPSTA = 1
!     ------------------------------------------------------
    if (mtpsta .eq. 1) then
        call asmpi_info(rank=mrank, size=msize)
        rang = to_aster_int(mrank)
        nbproc = to_aster_int(msize)
        ASSERT(nbproc.le.npromx)
        if (nbproc .gt. 1) then
            do 11,k=1,nbproc
            xtotap(k)=0.d0
            xsystp(k)=0.d0
            xelapp(k)=0.d0
11          continue
            xtotap(rang+1)=xtota
            xsystp(rang+1)=xsyst
            xelapp(rang+1)=xelap
            call asmpi_comm_vect('MPI_MAX', 'R', nbval=nbproc, vr=xtotap)
            call asmpi_comm_vect('MPI_MAX', 'R', nbval=nbproc, vr=xsystp)
            call asmpi_comm_vect('MPI_MAX', 'R', nbval=nbproc, vr=xelapp)
            call stati1(nbproc, xtotap, moyenn(1), ectype(1))
            call stati1(nbproc, xsystp, moyenn(2), ectype(2))
            call stati1(nbproc, xelapp, moyenn(3), ectype(3))
            if (lmesur) then
                nommel='    (MOYENNE    DIFF. PROCS)'
                write(ifm,1003) numes,nommel,moyenn(1),moyenn(2),&
                moyenn(3)
                nommel='    (ECART-TYPE DIFF. PROCS)'
                write(ifm,1003) numes,nommel,ectype(1),ectype(2),&
                ectype(3)
            endif
        endif
    endif
!
9999  continue
!
    1003 format (a8,a50,'CPU (USER+SYST/SYST/ELAPS):',3(f10.2))
end subroutine
