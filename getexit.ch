/* get:exitState values */
#ifdef A_HBGET
#define GE_NOEXIT       0       /* no exit attempted (blank) */
#define GE_UP           1
#define GE_DOWN         2
#define GE_TOP          3
#define GE_BOTTOM       4
#define GE_ENTER        5
#define GE_WRITE        6
#define GE_ESCAPE       7
#define GE_WHEN         8       /* when clause unsatisfied */
#define GE_SHORTCUT     9       /* introduced in x5.3 */
#define GE_MOUSEHIT     10      /* introduced in x5.3 */
#else
#define GE_NOEXIT     0      // no exit attempted (blank)
#define GE_LEFT       K_LEFT
#define GE_RIGHT      K_RIGHT
#define GE_UP         K_UP
#define GE_DOWN       K_DOWN
#define GE_TOP        K_CTRL_HOME
#define GE_BOTTOM     K_CTRL_END
#define GE_ENTER      K_ENTER
#define GE_WRITE      K_CTRL_L
#define GE_ESCAPE     K_ESC
#define GE_WHEN       K_CTRL_O      // when clause unsatisfied
#define GE_MOUSE      K_CTRL_N
#define GE_MOUSEHIT   K_CTRL_N      /* introduced in x5.3 */
#define GE_SHORTCUT   K_CTRL_P
#define GE_PGUP       K_PGUP
#define GE_PGDN       K_PGDN
#define GE_NOEDIT     K_CTRL_Q
#endif
#define _GETEXIT_CH

