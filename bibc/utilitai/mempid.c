/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2015  EDF R&D              WWW.CODE-ASTER.ORG */
/*                                                                    */
/* THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR      */
/* MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS     */
/* PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE */
/* LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.                    */
/* THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,    */
/* BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF     */
/* MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU   */
/* GENERAL PUBLIC LICENSE FOR MORE DETAILS.                           */
/*                                                                    */
/* YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE  */
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,      */
/*    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     */
/* ================================================================== */
/* person_in_charge: j-pierre.lefebvre at edf.fr */
#include "aster.h"

#ifdef _POSIX
# ifdef __FreeBSD__
#  include <kvm.h>
#  include <sys/param.h>
#  include <sys/sysctl.h>
#  include <sys/user.h>
#  include <err.h>
# endif
# include <fcntl.h>
#endif

/*
** Cette fonction permet de consulter le systeme de fichier /proc sous Unix
** et renvoie la memoire en octets consommee par le processus.
** Elle accede aux valeurs VmRSS et VmSize .
** La valeur retournee vaut VmStk.
**
** Le numero du processus est recupere par getpid
*/
INTEGER DEFP (MEMPID, mempid, INTEGER *val) 
{
    static char filename[80];
    static char sbuf[1024];
    char* S;
    int fd, num_read;
    long lmem;
    pid_t numpro;
#ifdef _POSIX
    pid_t getpid(void);

    numpro = getpid();

# ifdef __FreeBSD__
/*
** FreeBSD and some others without /proc ?
*/


#define B2K(x) ((x) >> 10) /* bytes to kbytes */
#define P2K(x) ((x) << (PAGE_SHIFT - 10)) /* pages to kbytes */

    char errbuf[_POSIX2_LINE_MAX];
    struct kinfo_proc *kp;
    kvm_t *kd;
    int count;
    kd = kvm_openfiles(NULL, "/dev/null", NULL, O_RDONLY, errbuf);
    if (kd == NULL)
        errx(1, "kvm_openfiles: %s", errbuf);

    kp = kvm_getprocs(kd, KERN_PROC_PID, numpro, &count);
    if (kp == NULL) {
        (void)fprintf(stderr, "kvm_getprocs: %s", kvm_geterr(kd));
        kvm_close(kd);
        return -1;
    }

    kvm_close(kd);

    /* VmData */
    val[0] = P2K((uintmax_t)kp->ki_dsize);
    /* VmSize */
    val[1] = B2K((uintmax_t)kp->ki_size);
    /* VmPeak - not defined in /compat/linux/proc/pid/status */
    val[2] = -1;
    /* VmRSS */
    val[3] = P2K((uintmax_t)kp->ki_rssize);
    /* VmStk */
    lmem = P2K((uintmax_t)kp->ki_ssize);

# else /* Linux */

    sprintf(filename, "/proc/%ld/status", (long)numpro);
    fd = open(filename, O_RDONLY, 0);
    if (fd==-1) return -1;
    num_read=read(fd,sbuf,(sizeof sbuf)-1);
/*  printf (" contenu du buffer = %s\n",sbuf); */   
    close(fd);

    S=strstr(sbuf,"VmData:")+8;
    val[0] = (INTEGER)atoi(S); 

    S=strstr(sbuf,"VmSize:")+8;
    val[1] = (INTEGER)atoi(S); 

    if ( strstr(sbuf,"VmPeak:") != NULL ) {
        S=strstr(sbuf,"VmPeak:")+8;
        val[2] = atoi(S);
    } else {
        val[2] = -1 ;  
    }

    S=strstr(sbuf,"VmRSS:")+7;
    val[3] = (INTEGER)atoi(S); 
      
    S=strstr(sbuf,"VmStk:")+7;
    lmem = atoi(S);
# endif

    return lmem ;

#else
/* 
** Pour retourner des valeurs sous Windows
*/
    val[0] = 0 ;
    val[1] = 0 ;
    val[2] = 0 ;
    val[3] = 0 ;
    return -1 ;
#endif
}
